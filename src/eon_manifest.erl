-module(eon_manifest).

-export([component_names/1, load/1, build/2]).

-export_type([manifest/0, project/0, component_type/0, component/0,
              escript/0, release/0]).

-type load_error_reason() ::
        empty_manifest_file
      | {invalid_manifest, Reason :: term()}.

-type build_error_reason() ::
        {unknown_component, atom()}
      | {unknown_component_type, atom()}
      | {unsupported_component_type, atom()}
      | {escript, eon_escript:error_reason()}.

-type manifest() ::
        #{root := file:filename_all(),
          components := #{atom() := component()}}.

-type project() ::
        #{name := unicode:chardata(),
          description => unicode:chardata()}.

-type component_type() ::
        escript
      | release.

-type component() ::
        #{type := component_type()}.

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
    components => #{}}.

-spec component_names(manifest()) -> [atom()].
component_names(#{components := Components}) ->
  maps:keys(Components).

-spec load(file:filename_all()) ->
        {ok, manifest()} | {error, load_error_reason()}.
load(Path) ->
  eon_log:debug(1, "loading manifest from ~ts", [Path]),
  case file:consult(Path) of
    {ok, []} ->
      {error, empty_manifest_file};
    {ok, [Manifest | _]} ->
      Root = filename:dirname(Path),
      Manifest0 = make(Root),
      {ok, maps:merge(Manifest0, Manifest)};
    {error, Reason} ->
      {error, {invalid_manifest, Reason}}
  end.

-spec build(ComponentName :: atom(), manifest()) ->
        {ok, ArtifactPath, Warnings} | {error, build_error_reason()} when
    ArtifactPath :: unicode:chardata(),
    Warnings :: list().
build(ComponentName, Manifest = #{components := Components}) ->
  case maps:find(ComponentName, Components) of
    {ok, Component = #{type := escript}} ->
      case eon_escript:build(Component, Manifest) of
        {ok, EscriptPath, Warnings} ->
          {ok, EscriptPath, Warnings};
        {error, Reason} ->
          {error, Reason}
      end;
    {ok, #{type := release}} ->
      {error, {unsupported_component_type, release}};
    {ok, #{type := Type}} ->
      {error, {unknown_component_type, Type}};
    error ->
      {error, {unknown_component, ComponentName}}
  end.
