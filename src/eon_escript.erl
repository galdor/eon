-module(eon_escript).

-export([build/2]).

-spec build(eon_manifest:escript(), eon_manifest:manifest()) ->
        file:filename_all().
build(Escript = #{type := escript,
                  name := Name,
                  main_module := MainModule},
      Manifest = #{root := Root}) ->
  OutputPath = filename:join([Root, Name]),
  BeamFiles = beam_files(Escript, Manifest),
  AppResourceFiles = application_resource_files(Escript, Manifest),
  %% escript:create/2 only supports strings for filenames
  Files = [eon_string:string(Path)
           || Path <- AppResourceFiles ++ BeamFiles],
  EbinPathArgs = [[" -pz ", Dir] || Dir <- file_directories(Files)],
  MiscArgs =
    %% Enable Unicode support for printable character detection
    [" -pc unicode"],
  EmuArgs = io_lib:format("-escript main ~ts~ts ~ts",
                          [MainModule, EbinPathArgs, MiscArgs]),
  Sections = [{shebang, "/usr/bin/env escript"},
              {emu_args, EmuArgs},
              {archive, Files, []}],
  case escript:create(OutputPath, Sections) of
    ok ->
      eon_fs:add_file_permissions(OutputPath, 8#0111),
      eon_fs:path(OutputPath);
    {error, Reason} ->
      throw({error, {escript_create, Reason}})
  end.

-spec beam_files(eon_manifest:escript(), eon_manifest:manifest()) ->
        [file:filename_all()].
beam_files(#{applications := Apps}, Manifest) ->
  lists:flatten([eon_app:beam_files(App, Manifest) || App <- Apps]).

-spec application_resource_files(eon_manifest:escript(),
                                 eon_manifest:manifest()) ->
        [eon_fs:path()].
application_resource_files(#{applications := Apps}, Manifest) ->
  [eon_app:resource_file_path(App, Manifest) || App <- Apps].

-spec file_directories([file:filename_all()]) -> [eon_fs:path()].
file_directories(Files) ->
  lists:map(fun eon_fs:path/1,
            maps:keys(#{filename:dirname(Filename) => true
                        || Filename <- Files})).
