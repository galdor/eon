-module(eon_app).

-export([resource_file/2, source_directory/2, source_files/2]).

-type error_reason() ::
        {resource_file_not_found, string()}
      | term().

-spec resource_file(atom(), eon_manifest:manifest()) ->
        {ok, binary()} | {error, error_reason()}.
resource_file(App, #{root := Root}) ->
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

-spec source_directory(atom(), eon_manifest:manifest()) ->
        {ok, binary()} | {error, error_reason()}.
source_directory(App, Manifest) ->
  case resource_file(App, Manifest) of
    {ok, Path} ->
      {ok, eon_fs:path(filename:dirname(Path))};
    {error, Reason} ->
      {error, Reason}
  end.

-spec source_files(atom(), eon_manifest:manifest()) ->
        {ok, [binary()]} | {error, error_reason()}.
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
