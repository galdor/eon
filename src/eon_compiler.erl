-module(eon_compiler).

-export([compile_file/3]).

-spec compile_file(file:filename_all(), ComponentName :: atom(),
                   eon_manifest:manifest()) -> ok.
compile_file(Filename, ComponentName, Manifest) ->
  eon_log:debug(1, "compiling ~ts", [Filename]),
  OutputDirectory = output_directory(Filename),
  eon_fs:ensure_directory(OutputDirectory),
  %% compile:file/2 only accepts strings
  FilenameString = eon_fs:path_string(Filename),
  OutputDirectoryString = eon_fs:path_string(OutputDirectory),
  AppCodePaths = eon_manifest:code_paths(ComponentName, Manifest),
  CodePath = code:get_path(),
  try
    code:add_paths([eon_fs:path_string(Path) || Path <- AppCodePaths]),
    Opts = [report_errors,
            report_warnings,
            {error_location, column},
            {outdir, OutputDirectoryString}],
    case compile:file(FilenameString, Opts) of
      {ok, _Mod} ->
        ok;
      error ->
        throw({error, compilation})
    end
  after
    code:set_path(CodePath)
  end.

-spec output_directory(file:filename_all()) -> eon_fs:path().
output_directory(Filename) ->
  %% All compiled files end up in <app>/ebin/<basename>.beam if the file is
  %% part of an application directory or ebin/<basename>.beam if it is a
  %% simplified project structure (no application, all source files in a
  %% top-level src directory).
  Path = eon_fs:path(Filename),
  case lists:reverse(filename:split(Path)) of
    [_Basename, Dir, AppName, <<"apps">> | Rest] when
        Dir =:= <<"src">>; Dir =:= <<"test">> ->
      filename:join(lists:reverse(Rest) ++ [<<"apps">>, AppName, <<"ebin">>]);
    [_Basename, Dir | Rest]  when
        Dir =:= <<"src">>; Dir =:= <<"test">> ->
      filename:join(lists:reverse(Rest) ++ [<<"ebin">>]);
    _ ->
      throw({error, {invalid_source_file_path, Filename}})
  end.
