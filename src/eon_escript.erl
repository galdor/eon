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
  Files = AppResourceFiles ++ BeamFiles,
  ArchiveFiles = [archive_file(Name, Path) || Path <- Files],
  Args =
    %% Enable Unicode support for printable character detection
    ["-pc unicode"],
  EmuArgs = io_lib:format(" -escript main ~ts ~ts", [MainModule, Args]),
  Sections = [{shebang, "/usr/bin/env escript"},
              {emu_args, EmuArgs},
              {archive, ArchiveFiles, []}],
  case escript:create(OutputPath, Sections) of
    ok ->
      eon_fs:add_file_permissions(OutputPath, 8#0111),
      eon_fs:path(OutputPath);
    {error, Reason} ->
      throw({error, {escript_create, Reason}})
  end.

-spec archive_file(eon_fs:path(), unicode:chardata()) ->
        {file:filename(), binary()}.
archive_file(ArchiveName, Path) ->
  %% From the documentation of the "code" module:
  %%
  %% At startup, both the top directory in the embedded archive as well as all
  %% (second level) ebin directories in the embedded archive are added to the
  %% code path.
  %%
  %% We use both "ebin/<filename>.beam" for simple projects and
  %% "<app>/ebin/<filename>.beam" for projects with nested applications.
  case file:read_file(Path) of
    {ok, Data} ->
      RelPath =
        case lists:reverse(filename:split(Path)) of
          [Filename, <<"ebin">>, AppName, <<"apps">> | _Rest] ->
            filename:join([ArchiveName, AppName, <<"ebin">>, Filename]);
          [Filename, <<"ebin">> | _Rest] ->
            filename:join([ArchiveName, <<"ebin">>, Filename])
        end,
      {eon_fs:path_string(RelPath), Data};
    {error, Reason} ->
      throw({error, {read_file, Path, Reason}})
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
