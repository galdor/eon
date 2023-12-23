-module(eon_compiler).

-export([compile_file/2]).

-spec compile_file(file:filename_all(), eon_manifest:manifest()) -> ok.
compile_file(Filename, _Manifest) ->
  eon_log:debug(1, "compiling ~ts", [Filename]),
  OutputDirectory = output_directory(Filename),
  eon_fs:ensure_directory(OutputDirectory),
  %% compile:file/2 only accepts strings
  FilenameString = eon_fs:path_string(Filename),
  OutputDirectoryString = eon_fs:path_string(OutputDirectory),
  Opts = [report_errors,
          report_warnings,
          {error_location, column},
          {outdir, OutputDirectoryString}],
  case compile:file(FilenameString, Opts) of
    {ok, _Mod} ->
      ok;
    error ->
      throw({error, compilation})
  end.

-spec output_directory(file:filename_all()) -> eon_fs:path().
output_directory(Filename) ->
  %% All compiled files end up in <app>/ebin/<basename>.beam if the file is
  %% part of an application directory or ebin/<basename>.beam if it is a
  %% simplified project structure (no application, all source files in a
  %% top-level src directory).
  Path = eon_fs:path(Filename),
  case lists:reverse(filename:split(Path)) of
    [_Basename, <<"src">>, AppName, <<"apps">> | Rest] ->
      filename:join(lists:reverse(Rest) ++ [<<"apps">>, AppName, <<"ebin">>]);
    [_Basename, <<"src">> | Rest] ->
      filename:join(lists:reverse(Rest) ++ [<<"ebin">>]);
    _ ->
      throw({error, {invalid_source_file_path, Filename}})
  end.
