-module(eon_app).

-export([source_directory/2, source_files/2, beam_files/2,
         generate_resource_file/2,
         resource_file_source_path/2, resource_file_path/2,
         compile/2]).

-export_type([error_reason/0]).

-type error_reason() ::
        {resource_file_not_found, string()}
      | {empty_resource_file, string()}
      | {invalid_resource_file, string(), Reason :: term()}
      | {invalid_resource_file_path, eon_fs:path()}
      | {missing_resource_file_property, atom()}
      | {write_file, eon_fs:path(), file:posix() | term()}
      | {external_program, [string()], eon_system:exec_error_reason()}
      | term().

%% TODO
-type specification() ::
        term().

-spec source_directory(atom(), eon_manifest:manifest()) ->
        {ok, binary()} | {error, error_reason()}.
source_directory(App, Manifest) ->
  case resource_file_source_path(App, Manifest) of
    {ok, Path} ->
      {ok, eon_fs:path(filename:dirname(Path))};
    {error, Reason} ->
      {error, Reason}
  end.

-spec source_files(atom(), eon_manifest:manifest()) ->
        {ok, [eon_fs:path()]} | {error, error_reason()}.
source_files(App, Manifest) ->
  case source_directory(App, Manifest) of
    {ok, DirPath} ->
      Filter = fun (Path) ->
                   {ok, filename:extension(Path) =:= <<".erl">>}
               end,
      eon_fs:find_files(DirPath, #{filter => Filter});
    {error, Reason} ->
      {error, Reason}
  end.

-spec beam_files(atom(), eon_manifest:manifest()) ->
        {ok, [eon_fs:path()]} | {error, error_reason()}.
beam_files(App, Manifest) ->
  case source_directory(App, Manifest) of
    {ok, SourceDirPath} ->
      BeamDirPath = filename:join(filename:dirname(SourceDirPath), "ebin"),
      Filter = fun (Path) ->
                   {ok, filename:extension(Path) =:= <<".beam">>}
               end,
      eon_fs:find_files(BeamDirPath, #{filter => Filter});
    {error, Reason} ->
      {error, Reason}
  end.

-spec generate_resource_file(atom(), eon_manifest:manifest()) ->
        {ok, eon_fs:path()} | {error, error_reason()}.
generate_resource_file(App, Manifest) ->
  case load_specification(App, Manifest) of
    {ok, Path, Specification} ->
      eon_log:debug(1, "processing ~ts", [Path]),
      case finalize_specification(Specification, Manifest) of
        {ok, Specification2} ->
          case resource_file_output_path(Path) of
            {ok, OutputPath} ->
              Data = io_lib:print(Specification2, 1, 80, -1),
              case file:write_file(OutputPath, Data) of
                ok ->
                  {ok, OutputPath};
                {error, Reason} ->
                  {error, {write_file, OutputPath, Reason}}
              end;
            {error, Reason} ->
              {error, Reason}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec load_specification(atom(), eon_manifest:manifest()) ->
        {ok, eon_fs:path(), specification()} | {error, error_reason()}.
load_specification(App, Manifest) ->
  case resource_file_source_path(App, Manifest) of
    {ok, Path} ->
      case file:consult(Path) of
        {ok, []} ->
          {error, {empty_resource_file, Path}};
        {ok, [Specification = {application, App, Properties} | _]} when
            is_list(Properties) ->
          {ok, Path, Specification};
        {ok, [{application, _, Properties} | _]} when is_list(Properties) ->
          {error, {invalid_specification, Path, application_name_mismatch}};
        {ok, [_Specification | _]} ->
          {error, {invalid_specification, Path, invalid_format}};
        {error, Reason} ->
          {error, {invalid_specification, Path, Reason}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec finalize_specification(specification(), eon_manifest:manifest()) ->
        {ok, specification()} | {error, error_reason()}.
finalize_specification(Specification, Manifest) ->
  Fun = fun
          Fun (S, []) ->
            {ok, S};
          Fun (S, [FinalizeFun | FinalizeFuns]) ->
            case FinalizeFun(S, Manifest) of
              {ok, S2} ->
                Fun(S2, FinalizeFuns);
              {error, Reason} ->
                {error, Reason}
            end
        end,
  Fun(Specification, [fun finalize_specification_modules/2,
                      fun finalize_specification_version/2]).

-spec finalize_specification_modules(specification(),
                                     eon_manifest:manifest()) ->
        {ok, specification()} | {error, error_reason()}.
finalize_specification_modules({application, App, Properties}, Manifest) ->
  case eon_app:source_files(App, Manifest) of
    {ok, Paths} ->
      Modules = [binary_to_atom(filename:basename(Path, ".erl"))
                 || Path <- Paths],
      Properties2 = lists:keystore(mods, 1, Properties, {mods, Modules}),
      {ok, {application, App, Properties2}};
    {error, Reason} ->
      {error, Reason}
  end.

-spec finalize_specification_version(specification(),
                                     eon_manifest:manifest()) ->
        {ok, specification()} | {error, error_reason()}.
finalize_specification_version(Specification = {application, App, Properties},
                               _Manifest) ->
  case lists:keyfind(vsn, 1, Properties) of
    {vsn, git} ->
      Program = "git",
      Args = ["describe", "--tags", "--dirty"],
      Options = #{first_line => true},
      case eon_system:exec(Program, Args, Options) of
        {ok, Version} ->
          Properties2 = lists:keystore(vsn, 1, Properties, {vsn, Version}),
          {ok, {application, App, Properties2}};
        {error, Reason} ->
          {error, {external_program, [Program | Args], Reason}}
      end;
    {vsn, _} ->
      {ok, Specification};
    false ->
      {error, {missing_resource_file_property, vsn}}
  end.

-spec resource_file_source_path(atom(), eon_manifest:manifest()) ->
        {ok, eon_fs:path()} | {error, error_reason()}.
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
      {ok, eon_fs:path(Path)};
    false ->
      {error, {resource_file_not_found, AppFilename}}
  end.

-spec resource_file_path(atom(), eon_manifest:manifest()) ->
        {ok, eon_fs:path()} | {error, error_reason()}.
resource_file_path(App, Manifest = #{root := Root}) ->
  case resource_file_source_path(App, Manifest) of
    {ok, Path} ->
      case resource_file_output_path(Path) of
        {ok, OutputPath} ->
          {ok, filename:join(Root, OutputPath)};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec resource_file_output_path(file:filename_all()) ->
        {ok, eon_fs:path()} | {error, error_reason()}.
resource_file_output_path(Filename) ->
  %% Application resource files are stored in <app>/ebin/<basename>.app if the
  %% file is part of an application directory or ebin/<basename>.app if it is
  %% a simplified project structure (no application, all source files in a
  %% top-level src directory).
  Path = eon_fs:path(Filename),
  Basename = filename:basename(Path, ".src"),
  case lists:reverse(filename:split(Path)) of
    [_Basename, <<"src">>, AppName | Rest] ->
      {ok, filename:join(lists:reverse(Rest) ++
                           [AppName, <<"ebin">>, Basename])};
    [_Basename, <<"src">> | Rest] ->
      {ok, filename:join(lists:reverse(Rest) ++ [<<"ebin">>, Basename])};
    _ ->
      {error, {invalid_source_file_path, Filename}}
  end.

-spec compile(atom(), eon_manifest:manifest()) ->
        {ok, Diagnostics} | {error, error_reason()} when
    Diagnostics :: [eon_compiler:diagnostic()].
compile(App, Manifest) ->
  case generate_resource_file(App, Manifest) of
    {ok, _Path} ->
      case source_files(App, Manifest) of
        {ok, Paths} ->
          compile_files(Paths, Manifest, []);
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec compile_files([eon_fs:path()], eon_manifest:manifest(), Diagnostics) ->
        {ok, Diagnostics} | {error, error_reason()} when
    Diagnostics :: [eon_compiler:diagnostic()].
compile_files([], _Manifest, Diagnostics) ->
  {ok, lists:flatten(Diagnostics)};
compile_files([Path | Paths], Manifest, Diagnostics) ->
  case eon_compiler:compile_file(Path, Manifest) of
    {ok, FileDiagnostics} ->
      compile_files(Paths, Manifest, Diagnostics ++ FileDiagnostics);
    {error, Reason} ->
      {error, Reason}
  end.
