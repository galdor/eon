-module(eon_manifest).

-export([component_names/1, load/1, build/2, compile/2]).

-export_type([manifest/0, project/0,
              dependency_type/0, dependency/0, git_dependency/0,
              component_type/0, component/0, library/0, escript/0, release/0]).

-type manifest() ::
        #{root := file:filename_all(),
          dependencies := #{atom() := dependency()},
          components := #{atom() := component()}}.

-type project() ::
        #{name := unicode:chardata(),
          description => unicode:chardata()}.

-type dependency_type() ::
        git.

-type dependency() ::
        #{type := dependency_type()}.

-type git_dependency() ::
        #{type := git,
          repository := string(),
          ref => string(),
          subdirectory => string()}.

-type component_type() ::
        library
      | escript
      | release.

-type component() ::
        #{type := component_type()}.

-type library() ::
        #{type := library,
          applications := [atom()]}.

-type escript() ::
        #{type := escript,
          name := string(),
          main_module := atom(),
          applications := [atom()]}.

-type release() ::
        #{type := release,
          name := string(),
          version := string(),
          applications := [atom()]}.

-spec make(file:filename_all()) -> manifest().
make(Root) ->
  #{root => Root,
    dependencies => #{},
    components => #{}}.

-spec component_names(manifest()) -> [atom()].
component_names(#{components := Components}) ->
  maps:keys(Components).

-spec load(file:filename_all()) -> manifest().
load(Path) ->
  eon_log:debug(1, "loading manifest from ~ts", [Path]),
  case file:consult(Path) of
    {ok, []} ->
      throw({error, {empty_manifest, Path}});
    {ok, [Manifest | _]} ->
      Root = filename:dirname(Path),
      Manifest0 = make(Root),
      maps:merge(Manifest0, Manifest);
    {error, Reason} ->
      throw({error, {invalid_manifest, Path, Reason}})
  end.

-spec build(ComponentName :: atom(), manifest()) -> eon_fs:path().
build(ComponentName, Manifest = #{components := Components}) ->
  case maps:find(ComponentName, Components) of
    {ok, Component = #{type := escript}} ->
      eon_escript:build(Component, Manifest);
    {ok, #{type := Type}} when Type =:= library; Type =:= release ->
      throw({error, {unsupported_component_type, Type}});
    {ok, #{type := Type}} ->
      throw({error, {unknown_component_type, Type}});
    error ->
      throw({error, {unknown_component, ComponentName}})
  end.

-spec compile(ComponentName :: atom(), manifest()) -> ok.
compile(ComponentName, Manifest = #{components := Components}) ->
  case maps:find(ComponentName, Components) of
    {ok, Component = #{type := Type}} when
        Type =:= library; Type =:= escript; Type =:= release ->
      Apps = maps:get(applications, Component, []),
      lists:foreach(fun (App) -> eon_app:compile(App, Manifest) end, Apps);
    {ok, #{type := library}} ->
      throw({error, {unsupported_component_type, library}});
    {ok, #{type := Type}} ->
      throw({error, {unknown_component_type, Type}});
    error ->
      throw({error, {unknown_component, ComponentName}})
  end.
