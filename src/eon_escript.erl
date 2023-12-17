-module(eon_escript).

-export([build/2]).

-export_type([error_reason/0]).

-type error_reason() ::
        {unknown_escript, string()}
      | {build, term()}.

-type file() ::
        {file:name(), binary()}.

-spec build(eon_manifest:escript(), eon_manifest:manifest()) ->
        {ok, file:filename_all()} | {error, error_reason()}.
build(Escript = #{type := escript,
                  name := Name,
                  main_module := MainModule},
      Manifest = #{root := Root}) ->
  OutputPath = filename:join([Root, Name]),
  case beam_files(Escript, Manifest) of
    {ok, BeamFiles} ->
      case application_resource_files(Escript, Manifest) of
        {ok, AppResourceFiles} ->
          %% escript:create/2 only supports strings for filenames
          Files = [eon_string:string(Path)
                   || Path <- AppResourceFiles ++ BeamFiles],
          EbinPathArgs = [[" -pz ", Dir] || Dir <- file_directories(Files)],
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
              {ok, eon_fs:path(OutputPath)};
            {error, Reason} ->
              {error, {build, Reason}}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec beam_files(eon_manifest:escript(), eon_manifest:manifest()) ->
        {ok, [file:filename_all()]} | {error, error_reason()}.
beam_files(#{applications := Apps}, Manifest) ->
  beam_files(Apps, Manifest, []).

-spec beam_files([atom()], eon_manifest:manifest(), [file:filename_all()]) ->
        {ok, [eon_fs:path()]} | {error, error_reason()}.
beam_files([], _, Acc) ->
  {ok, lists:flatten(Acc)};
beam_files([App | Apps], Manifest, Acc) ->
  case eon_app:beam_files(App, Manifest) of
    {ok, Paths} ->
      beam_files(Apps, Manifest, [Paths| Acc]);
    {error, Reason} ->
      {error, Reason}
  end.

-spec application_resource_files(eon_manifest:escript(),
                                 eon_manifest:manifest()) ->
        {ok, [file:filename_all()]} | {error, error_reason()}.
application_resource_files(#{applications := Apps}, Manifest) ->
  application_resource_files(Apps, Manifest, []).

-spec application_resource_files(eon_manifest:escript(),
                                 eon_manifest:manifest(),
                                 [file:filename_all()]) ->
        {ok, [file:filename_all()]} | {error, error_reason()}.
application_resource_files([], _Manifest, Paths) ->
  {ok, lists:reverse(Paths)};
application_resource_files([App | Apps], Manifest, Paths) ->
  case eon_app:resource_file_path(App, Manifest) of
    {ok, Path} ->
      application_resource_files(Apps, Manifest, [Path | Paths]);
    {error, Reason} ->
      {error, Reason}
  end.

-spec file_directories(file()) -> [file:name()].
file_directories(Files) ->
  maps:keys(#{filename:dirname(Filename) => true || Filename <- Files}).
