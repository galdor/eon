-module(eon_escript).

-export([build/2,
         source_files/2]).

-export_type([error_reason/0]).

-type error_reason() ::
        {unknown_escript, string()}
      | {compilation, eon_compiler:error_reason()}
      | {build, term()}.

-type file() ::
        {file:name(), binary()}.

-spec build(eon_manifest:escript(), eon_manifest:manifest()) ->
        {ok, file:filename_all(), Warnings} | {error, error_reason()} when
    Warnings :: [eon_compiler:diagnostic()].
build(Escript = #{type := escript,
                  name := Name,
                  main_module := MainModule},
      Manifest = #{root := Root}) ->
  OutputPath = filename:join([Root, Name]),
  case generate_application_resource_files(Escript, Manifest) of
    {ok, AppResourceFiles} ->
      case source_files(Escript, Manifest) of
        {ok, SourcePaths} ->
          case compile_files(SourcePaths, Manifest, [], []) of
            {ok, BeamFiles, Warnings} ->
              %% escript:create/2 uses zip:create/2 for the archive and it
              %% does not support io lists. So we have to convert application
              %% resource file data (which are made of textual data) to
              %% binaries.
              AppResourceFiles2 = [{Path, eon_string:binary(Data)}
                                   || {Path, Data} <- AppResourceFiles],
              %% escript:create/2 only supports strings for filenames
              Files = [{eon_string:string(Path), Data}
                       || {Path, Data} <- AppResourceFiles2 ++ BeamFiles],
              EbinPathArgs = [[" -pz ", Dir]
                              || Dir <- file_directories(Files)],
              MiscArgs =
                %% Enable Unicode support for printable character detection
                [" -pc unicode"],
              EmuArgs = io_lib:format("-escript main ~ts~ts ~ts",
                                      [MainModule, EbinPathArgs, MiscArgs]),
              Sections =
                [{shebang, "/usr/bin/env escript"},
                 {emu_args, EmuArgs},
                 {archive, Files, []}],
              case escript:create(OutputPath, Sections) of
                ok ->
                  ok = eon_fs:add_file_permissions(OutputPath, 8#0111),
                  {ok, OutputPath, Warnings};
                {error, Reason} ->
                  {error, {build, Reason}}
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

-spec generate_application_resource_files(eon_manifest:escript(),
                                          eon_manifest:manifest()) ->
        {ok, [file()]} | {error, error_reason()}.
generate_application_resource_files(#{applications := Apps}, Manifest) ->
  generate_application_resource_files(Apps, Manifest, []).

-spec generate_application_resource_files([atom()], eon_manifest:manifest(),
                                          [file()]) ->
        {ok, [file()]} | {error, error_reason()}.
generate_application_resource_files([], _, Acc) ->
  {ok, lists:reverse(Acc)};
generate_application_resource_files([App | Apps], Manifest, Acc) ->
  case eon_app:generate_resource_file(App, Manifest) of
    {ok, File} ->
      generate_application_resource_files(Apps, Manifest, [File | Acc]);
    {error, Reason} ->
      {error, Reason}
  end.

-spec source_files(eon_manifest:escript(), eon_manifest:manifest()) ->
        {ok, [file:filename_all()]} | {error, error_reason()}.
source_files(#{applications := Apps}, Manifest) ->
  source_files(Apps, Manifest, []).

-spec source_files([atom()], eon_manifest:manifest(), [file:filename_all()]) ->
        {ok, [eon_fs:path()]} | {error, error_reason()}.
source_files([], _, Acc) ->
  {ok, lists:flatten(Acc)};
source_files([App | Apps], Manifest, Acc) ->
  case eon_app:source_files(App, Manifest) of
    {ok, Paths} ->
      source_files(Apps, Manifest, [Paths| Acc]);
    {error, Reason} ->
      {error, Reason}
  end.

-spec compile_files([eon_fs:path()], eon_manifest:manifest(),
                    [file()], [eon_compiler:diagnostic()]) ->
        {ok, [file()], [eon_compiler:diagnostic()]} |
        {error, error_reason()}.
compile_files([], _Manifest, BeamFiles, Warnings) ->
  {ok, lists:reverse(BeamFiles), Warnings};
compile_files([Path | Paths], Manifest, BeamFiles, Warnings) ->
  case eon_compiler:compile_file(Path, Manifest) of
    {ok, BeamFile, Warnings2} ->
      compile_files(Paths, Manifest, [BeamFile | BeamFiles],
                    Warnings ++ Warnings2);
    {error, Reason} ->
      {error, {compilation, Reason}}
  end.

-spec file_directories(file()) -> [file:name()].
file_directories(Files) ->
  maps:keys(#{filename:dirname(Filename) => true || {Filename, _} <- Files}).
