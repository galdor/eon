-module(eon_fs).

-export([file_exists/1,
         add_file_permissions/2,
         find_files/1, find_files/2,
         ensure_directory/1,
         path/1, path_string/1]).

-export_type([path/0]).

-include_lib("kernel/include/file.hrl").

-type path() :: binary().

-type find_files_filter() ::
        fun((file:filename_all()) -> {ok, boolean()} | {error, term()}).

-type find_files_options() ::
        #{filter => find_files_filter()}.

-spec file_exists(file:filename_all()) ->
        {ok, boolean()} | {error, Reason} when
    Reason :: file:posix() | badarg.
file_exists(Path) ->
  case file:read_file_info(Path) of
    {ok, _} ->
      {ok, true};
    {error, enoent} ->
      {ok, false};
    {error, Reason} ->
      {error, Reason}
  end.

-spec add_file_permissions(file:filename_all(), non_neg_integer()) -> ok.
add_file_permissions(Path, Mask) ->
  case file:read_file_info(Path) of
    {ok, #file_info{mode = Mode}} ->
      file:change_mode(Path, Mode bor Mask);
    {error, Reason} ->
      throw({error, {read_file_info, Path, Reason}})
  end.

-spec find_files(file:filename_all()) -> [file:filename_all()].
find_files(DirPath) ->
  find_files(DirPath, #{}).

-spec find_files(file:filename_all(), find_files_options()) ->
        [file:filename_all()].
find_files(DirPath, Opts) ->
  Paths = find_files(path(DirPath), Opts, []),
  lists:flatten(Paths).

-spec find_files(path(), find_files_options(), [path()]) ->
        [file:filename_all()].
find_files(Path, Opts, Acc) ->
  case file:read_file_info(Path) of
	{ok, #file_info{type = directory}} ->
      case file:list_dir(Path) of
        {ok, Paths} ->
          [[find_files(filename:join(Path, P), Opts, []) || P <- Paths] | Acc];
        {error, Reason} ->
          throw({error, {list_directory, Path, Reason}})
      end;
	{ok, _} ->
      case Opts of
        #{filter := Filter} ->
          case Filter(Path) of
            true  -> [Path | Acc];
            false -> Acc
          end;
        _ ->
          [Path | Acc]
      end;
    {error, Reason} ->
      throw({error, {file_info, Path, Reason}})
  end.

-spec ensure_directory(file:filename_all()) -> ok.
ensure_directory(Directory) ->
  %% filelib:ensure_dir/1 only creates the parent directories while
  %% filelib:ensure_path/1 actually creates the entire path. Yes the name of
  %% the function does not make any sense.
  case filelib:ensure_path(Directory) of
    ok ->
      ok;
    {error, Reason} ->
      throw({error, {ensure_path, Directory, Reason}})
  end.

-spec path(file:filename_all()) -> path().
path(Filename) when is_list(Filename) ->
  list_to_binary(Filename);
path(Filename) when is_binary(Filename) ->
  Filename.

-spec path_string(file:filename_all()) -> string().
path_string(Filename) when is_list(Filename) ->
  Filename;
path_string(Filename) when is_binary(Filename) ->
  eon_string:string(Filename).
