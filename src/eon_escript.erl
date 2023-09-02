-module(eon_escript).

-export([build/2,
         source_files/2]).

-export_type([error_reason/0]).

-type error_reason() ::
        {unknown_escript, string()}
      | {compilation, eon_compiler:error_reason()}
      | {build, term()}.

-spec build(eon_manifest:escript(), eon_manifest:manifest()) ->
        {ok, file:filename_all(), Warnings} | {error, error_reason()} when
    Warnings :: [eon_compiler:diagnostic()].
build(Escript = #{type := escript,
                  name := Name,
                  main_module := MainModule},
      Manifest = #{root := Root}) ->
  OutputPath = filename:join([Root, Name]),
  case source_files(Escript, Manifest) of
    {ok, SourcePaths} ->
      case compile_files(SourcePaths, Manifest, [], []) of
        {ok, BeamFiles, Warnings} ->
          EbinPathArgs = [[" -pz ", Dir]
                          || Dir <- beam_file_directories(BeamFiles)],
          EmuArgs = io_lib:format("-escript main ~ts~ts",
                                  [MainModule, EbinPathArgs]),
          Sections =
            [{shebang, "/usr/bin/env escript"},
             {emu_args, EmuArgs},
             {archive, BeamFiles, []}],
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
  end.

-spec source_files(eon_manifest:escript(), eon_manifest:manifest()) ->
        {ok, [file:filename_all()]} | {error, error_reason()}.
source_files(#{applications := Apps}, Manifest) ->
  source_files(Apps, Manifest, []).

-spec source_files([atom()], eon_manifest:manifest(), [file:filename_all()]) ->
        {ok, [binary()]} | {error, error_reason()}.
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
                    BeamFiles, [eon_compiler:diagnostic()]) ->
        {ok, BeamFiles, [eon_compiler:diagnostic()]} |
        {error, error_reason()} when
    BeamFiles :: [{file:name(), binary()}].
compile_files([], _Manifest, BeamFiles, Warnings) ->
  {ok, lists:reverse(BeamFiles), Warnings};
compile_files([Path | Paths], Manifest, BeamFiles, Warnings) ->
  case eon_compiler:compile_file(Path, Manifest) of
    {ok, {BeamPath, BeamData}, Warnings2} ->
      compile_files(Paths, Manifest,
                    [{eon_fs:path_string(BeamPath), BeamData} | BeamFiles],
                    Warnings ++ Warnings2);
    {error, Reason} ->
      {error, {compilation, Reason}}
  end.

-spec beam_file_directories(BeamFiles) -> [file:name()] when
    BeamFiles :: [{file:name(), binary()}].
beam_file_directories(BeamFiles) ->
  maps:keys(#{filename:dirname(Filename) => true
              || {Filename, _} <- BeamFiles}).
