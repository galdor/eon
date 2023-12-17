-module(eon_compiler).

-export([compile_file/2]).

-export_type([error_reason/0,
              diagnostic/0,
              position/0, line/0, column/0]).

-type error_reason() ::
        {diagnostics, [diagnostic()]}
      | {invalid_source_file_path, file:filename_all()}
      | {ensure_directory, eon_fs:path(), file:posix()}.

-type diagnostic() ::
        {eon_fs:path(), error | warning, position(), term()}.

-type position() :: {line(), column()} | none.
-type line() :: pos_integer().
-type column() :: pos_integer().

-spec compile_file(file:filename_all(), eon_manifest:manifest()) ->
        {ok, Diagnostics} | {error, error_reason()} when
    Diagnostics :: [diagnostic()].
compile_file(Filename, _Manifest = #{root := Root}) ->
  eon_log:debug(1, "compiling ~ts", [Filename]),
  case output_directory(Filename) of
    {ok, RelOutputDirectory} ->
      OutputDirectory = filename:join(Root, RelOutputDirectory),
      case eon_fs:ensure_directory(OutputDirectory) of
        ok ->
          %% compile:file/2 only accepts strings
          FilenameString = eon_fs:path_string(Filename),
          OutputDirectoryString = eon_fs:path_string(OutputDirectory),
          Opts = [return_errors,
                  return_warnings,
                  {error_location, column},
                  {outdir, OutputDirectoryString}],
          case compile:file(FilenameString, Opts) of
            {ok, _Mod} ->
              {ok, []};
            {ok, _Mod, Warnings} ->
              {ok, diagnostics(Warnings, warning, [])};
            {error, Errors, Warnings} ->
              ErrorDiagnostics = diagnostics(Errors, error, []),
              WarningDiagnostics = diagnostics(Warnings, warning, []),
              Diagnostics =  ErrorDiagnostics ++ WarningDiagnostics,
              {error, {diagnostics, Diagnostics}}
          end;
        {error, Reason} ->
          {error, {ensure_directory, OutputDirectory, Reason}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec output_directory(file:filename_all()) ->
        {ok, eon_fs:path()} | {error, error_reason()}.
output_directory(Filename) ->
  %% All compiled files end up in <app>/ebin/<basename>.beam if the file is
  %% part of an application directory or ebin/<basename>.beam if it is a
  %% simplified project structure (no application, all source files in a
  %% top-level src directory).
  Path = eon_fs:path(Filename),
  case lists:reverse(filename:split(Path)) of
    [_Basename, <<"src">>, <<".">>] ->
      {ok, filename:join(lists:reverse([<<"ebin">>]))};
    [_Basename, <<"src">>, AppName | _Rest] ->
      {ok, filename:join(lists:reverse([<<"ebin">>, AppName]))};
    _ ->
      {error, {invalid_source_file_path, Filename}}
  end.

-spec diagnostics(compile:errors(), error | warning, [diagnostic()]) ->
        [diagnostic()].
diagnostics([], _Type, Acc) ->
  lists:flatten(Acc);
diagnostics([{Filename, Infos} | Errors], Type, Acc) ->
  diagnostics(Errors, Type,
              [[{eon_fs:path(Filename), Type, Location, Description} ||
                 {Location, _Module, Description} <- Infos] |
               Acc]).
