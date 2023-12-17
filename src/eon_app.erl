-module(eon_app).

-export([source_directory/2, source_files/2, beam_files/2,
         generate_resource_file/2,
         resource_file_source_path/2, resource_file_path/2,
         compile/2]).

%% TODO
-type specification() ::
        term().

-spec source_directory(atom(), eon_manifest:manifest()) -> binary().
source_directory(App, Manifest) ->
  Path = resource_file_source_path(App, Manifest),
  eon_fs:path(filename:dirname(Path)).

-spec source_files(atom(), eon_manifest:manifest()) -> [eon_fs:path()].
source_files(App, Manifest) ->
  DirPath = source_directory(App, Manifest),
  Filter = fun (Path) ->
               filename:extension(Path) =:= <<".erl">>
           end,
  eon_fs:find_files(DirPath, #{filter => Filter}).

-spec beam_files(atom(), eon_manifest:manifest()) -> [eon_fs:path()].
beam_files(App, Manifest) ->
  SourceDirPath = source_directory(App, Manifest),
  BeamDirPath = filename:join(filename:dirname(SourceDirPath), "ebin"),
  Filter = fun (Path) ->
               filename:extension(Path) =:= <<".beam">>
           end,
  eon_fs:find_files(BeamDirPath, #{filter => Filter}).

-spec generate_resource_file(atom(), eon_manifest:manifest()) -> eon_fs:path().
generate_resource_file(App, Manifest) ->
  {Path, Specification} = load_specification(App, Manifest),
  eon_log:debug(1, "processing ~ts", [Path]),
  Specification2 = finalize_specification(Specification, Manifest),
  OutputPath = resource_file_output_path(Path),
  Data = io_lib:print(Specification2, 1, 80, -1),
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
      Properties2 = lists:keystore(vsn, 1, Properties, {vsn, Version}),
      {application, App, Properties2};
    {vsn, _} ->
      Specification;
    false ->
      throw({error, {missing_resource_file_property, vsn}})
  end.

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
resource_file_path(App, Manifest = #{root := Root}) ->
  Path = resource_file_source_path(App, Manifest),
  OutputPath = resource_file_output_path(Path),
  filename:join(Root, OutputPath).

-spec resource_file_output_path(file:filename_all()) -> eon_fs:path().
resource_file_output_path(Filename) ->
  %% Application resource files are stored in <app>/ebin/<basename>.app if the
  %% file is part of an application directory or ebin/<basename>.app if it is
  %% a simplified project structure (no application, all source files in a
  %% top-level src directory).
  Path = eon_fs:path(Filename),
  Basename = filename:basename(Path, ".src"),
  case lists:reverse(filename:split(Path)) of
    [_Basename, <<"src">>, AppName | Rest] ->
      filename:join(lists:reverse(Rest) ++ [AppName, <<"ebin">>, Basename]);
    [_Basename, <<"src">> | Rest] ->
      filename:join(lists:reverse(Rest) ++ [<<"ebin">>, Basename]);
    _ ->
      throw({error, {invalid_source_file_path, Filename}})
  end.

-spec compile(atom(), eon_manifest:manifest()) -> [eon_compiler:diagnostic()].
compile(App, Manifest) ->
  _Path = generate_resource_file(App, Manifest),
  Paths = source_files(App, Manifest),
  lists:flatten([eon_compiler:compile_file(Path, Manifest) || Path <- Paths]).
