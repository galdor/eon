-module(eon_app).

-export([source_directory/2, source_files/2, generate_resource_file/2]).

-type error_reason() ::
        {resource_file_not_found, string()}
      | {empty_resource_file, string()}
      | {invalid_resource_file, string(), Reason :: term()}
      | {invalid_resource_file_path, eon_fs:path()}
      | term().

%% TODO
-type specification() ::
        term().

-spec source_directory(atom(), eon_manifest:manifest()) ->
        {ok, binary()} | {error, error_reason()}.
source_directory(App, Manifest) ->
  case resource_file_path(App, Manifest) of
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

-spec generate_resource_file(atom(), eon_manifest:manifest()) ->
        {ok, {eon_fs:path(), iodata()}} | {error, error_reason()}.
generate_resource_file(App, Manifest) ->
  case load_specification(App, Manifest) of
    {ok, Path, Specification} ->
      eon_log:debug(1, "processing ~ts", [Path]),
      case finalize_specification(Specification, Manifest) of
        {ok, Specification2} ->
          case resource_file_output_path(Path) of
            {ok, OutputPath} ->
              Data = io_lib:print(Specification2, 1, 80, -1),
              {ok, {OutputPath, [Data, $.]}};
            {error, Reason} ->
              {error, Reason}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec load_specification(atom(), eon_manifest:manifest()) ->
        {ok, eon_fs:path(), specification()} | {error, error_reason()}.
load_specification(App, Manifest) ->
  case resource_file_path(App, Manifest) of
    {ok, Path} ->
      case file:consult(Path) of
        {ok, []} ->
          {error, {empty_resource_file, Path}};
        {ok, [Specification | _]} ->
          {ok, Path, Specification};
        {error, Reason} ->
          {error, {invalid_specification, Path, Reason}}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec finalize_specification(specification(), eon_manifest:manifest()) ->
        {ok, specification()} | {error, error_reason()}.
finalize_specification(Specification, _Manifest) ->
  %% TODO
  {ok, Specification}.

-spec resource_file_path(atom(), eon_manifest:manifest()) ->
        {ok, eon_fs:path()} | {error, error_reason()}.
resource_file_path(App, #{root := Root}) ->
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

-spec resource_file_output_path(file:filename_all()) ->
        {ok, eon_fs:path()} | {error, error_reason()}.
resource_file_output_path(Filename) ->
  %% Application resource files are stored in <app>/ebin/<basename>.app if the
  %% file is part of an application directory or ebin/<basename>.app if it is
  %% a simplified project structure (no application, all source files in a
  %% top-level src directory).
  Path = eon_fs:path(Filename),
  Basename = filename:basename(Path, ".src"),
  case lists:reverse(filename:split(Path)) of
    [_Basename, <<"src">>, <<".">>] ->
      {ok, filename:join(lists:reverse([<<"ebin">>]) ++ [Basename])};
    [_Basename, <<"src">>, AppName | _Rest] ->
      {ok, filename:join(lists:reverse([<<"ebin">>, AppName]) ++ [Basename])};
    _ ->
      {error, {invalid_source_file_path, Filename}}
  end.
