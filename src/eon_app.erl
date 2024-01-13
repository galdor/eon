-module(eon_app).

-export([source_files/2, beam_files/2,
         generate_resource_file/2,
         path/2, src_path/2, test_path/2, ebin_path/2,
         resource_file_source_path/2, resource_file_path/2,
         compile/3]).

%% TODO
-type specification() ::
        term().

-spec source_files(atom(), eon_manifest:manifest()) -> [eon_fs:path()].
source_files(App, Manifest) ->
  Filter = eon_fs:find_files_extension_filter(<<".erl">>),
  SrcFiles = eon_fs:find_files(src_path(App, Manifest), #{filter => Filter}),
  TestPath = test_path(App, Manifest),
  TestFiles =
    case filelib:is_dir(TestPath) of
      true ->  eon_fs:find_files(TestPath, #{filter => Filter});
      false -> []
    end,
  lists:append([SrcFiles, TestFiles]).

-spec beam_files(atom(), eon_manifest:manifest()) -> [eon_fs:path()].
beam_files(App, Manifest) ->
  Filter = eon_fs:find_files_extension_filter(<<".beam">>),
  eon_fs:find_files(ebin_path(App, Manifest), #{filter => Filter}).

-spec generate_resource_file(atom(), eon_manifest:manifest()) -> eon_fs:path().
generate_resource_file(App, Manifest) ->
  {SourcePath, Specification} = load_specification(App, Manifest),
  eon_log:debug(1, "processing ~ts", [SourcePath]),
  Specification2 = finalize_specification(Specification, Manifest),
  OutputPath = resource_file_path(App, Manifest),
  eon_fs:ensure_directory(filename:dirname(OutputPath)),
  Data = [io_lib:print(Specification2, 1, 80, -1), $.],
  case file:write_file(OutputPath, Data) of
    ok ->
      OutputPath;
    {error, Reason} ->
      throw({error, {write_file, OutputPath, Reason}})
  end.

-spec load_specification(atom(), eon_manifest:manifest()) ->
        {eon_fs:path(), specification()}.
load_specification(App, Manifest) ->
  Path = resource_file_source_path(App, Manifest),
  case file:consult(Path) of
    {ok, []} ->
      throw({error, {empty_resource_file, Path}});
    {ok, [Specification = {application, App, Properties} | _]} when
        is_list(Properties) ->
      {Path, Specification};
    {ok, [{application, _, Properties} | _]} when is_list(Properties) ->
      throw({error, {invalid_specification, Path, application_name_mismatch}});
    {ok, [_Specification | _]} ->
      throw({error, {invalid_specification, Path, invalid_format}});
    {error, Reason} ->
      throw({error, {invalid_specification, Path, Reason}})
  end.

-spec finalize_specification(specification(), eon_manifest:manifest()) ->
        specification().
finalize_specification(Specification0, Manifest) ->
  Funs = [fun finalize_specification_modules/2,
          fun finalize_specification_version/2],
  lists:foldl(fun (Fun, Specification) ->
                  Fun(Specification, Manifest)
              end, Specification0, Funs).

-spec finalize_specification_modules(specification(),
                                     eon_manifest:manifest()) ->
        specification().
finalize_specification_modules({application, App, Properties}, Manifest) ->
  Paths = eon_app:source_files(App, Manifest),
  Modules = [binary_to_atom(filename:basename(Path, ".erl")) || Path <- Paths],
  Properties2 = lists:keystore(mods, 1, Properties, {mods, Modules}),
  {application, App, Properties2}.

-spec finalize_specification_version(specification(),
                                     eon_manifest:manifest()) ->
        specification().
finalize_specification_version(Specification = {application, App, Properties},
                               _Manifest) ->
  case lists:keyfind(vsn, 1, Properties) of
    {vsn, git} ->
      Program = "git",
      Args = ["describe", "--tags", "--dirty"],
      Options = #{first_line => true},
      Version = eon_system:exec(Program, Args, Options),
      VersionString = eon_string:string(Version),
      Properties2 = lists:keystore(vsn, 1, Properties, {vsn, VersionString}),
      {application, App, Properties2};
    {vsn, _} ->
      Specification;
    false ->
      throw({error, {missing_resource_file_property, vsn}})
  end.

-spec path(atom(), eon_manifest:manifest()) -> eon_fs:path().
path(App, Manifest) ->
  ResourceFilePath = resource_file_source_path(App, Manifest),
  SrcPath = filename:dirname(ResourceFilePath),
  filename:dirname(SrcPath).

-spec src_path(atom(), eon_manifest:manifest()) -> eon_fs:path().
src_path(App, Manifest) ->
  filename:join(path(App, Manifest), <<"src">>).

-spec test_path(atom(), eon_manifest:manifest()) -> eon_fs:path().
test_path(App, Manifest) ->
  filename:join(path(App, Manifest), <<"test">>).

-spec ebin_path(atom(), eon_manifest:manifest()) -> eon_fs:path().
ebin_path(App, Manifest) ->
  filename:join(path(App, Manifest), <<"ebin">>).

-spec resource_file_source_path(atom(), eon_manifest:manifest()) ->
        eon_fs:path().
resource_file_source_path(App, #{root := Root}) ->
  AppName = erlang:atom_to_list(App),
  AppFilename = AppName ++ ".app.src",
  Paths = [filename:join([Root, "src", AppFilename]),
           filename:join([Root, "apps", AppName, "src", AppFilename])],
  case
    lists:search(fun (Path) ->
                     case eon_fs:file_exists(Path) of
                       {ok, true} ->
                         true;
                       {ok, false} ->
                         false;
                       {error, Reason} ->
                         throw({error, Reason})
                     end
                 end, Paths)
  of
    {value, Path} ->
      eon_fs:path(Path);
    false ->
      throw({error, {resource_file_not_found, AppFilename}})
  end.

-spec resource_file_path(atom(), eon_manifest:manifest()) -> eon_fs:path().
resource_file_path(App, Manifest) ->
  Filename = <<(atom_to_binary(App))/binary, ".app">>,
  filename:join(ebin_path(App, Manifest), Filename).

-spec compile(atom(), ComponentName :: atom(), eon_manifest:manifest()) -> ok.
compile(App, ComponentName, Manifest) ->
  generate_resource_file(App, Manifest),
  Paths = source_files(App, Manifest),
  lists:foreach(fun (Path) ->
                    eon_compiler:compile_file(Path, ComponentName, Manifest)
                end, Paths).
