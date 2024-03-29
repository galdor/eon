-module(eon).

-export([main/1]).

-export_type([test_cfg/0]).

-type command() ::
        build
      | compile
      | help
      | shell
      | test.

-type command_line_data() ::
        #{command := command(),
          arguments := [binary()],
          options := command_line_options()}.

-type command_line_options() ::
        #{atom() := boolean() | binary()}.

-type test_cfg() ::
        #{component := atom(),
          verbose => boolean()}.

main(Args) ->
  process_flag(trap_exit, true),
  case parse_command_line(Args) of
    #{command := help} ->
      usage();
    #{options := #{help := true}} ->
      usage();
    CommandLineData = #{command := Command, options := Options} ->
      LogLevel = case maps:get(verbose, Options, false) of
                   true -> info;
                   false -> error
                 end,
      eon_log:start(#{level => LogLevel}),
      Manifest = load_manifest(Options),
      case Command of
        compile ->
          cmd_compile(CommandLineData, Manifest);
        build ->
          cmd_build(CommandLineData, Manifest);
        shell ->
          cmd_shell(CommandLineData, Manifest);
        test ->
          cmd_test(CommandLineData, Manifest)
      end,
      eon_log:stop()
  end.

usage() ->
  ProgramName = escript:script_name(),
  io:format(<<"Usage: ~s OPTIONS <command>~n"
              "~n"
              "OPTIONS~n"
              "~n"
              "-C <path>               set the root directory of the project"
              " (default: .)~n"
              "-h                      print help and exit~n"
              "-v                      print informational messages~n"
              "~n"
              "COMMANDS~n"
              "~n"
              "build                   build all components~n"
              "build <component>...    build one or more components~n"
              "compile <component>...  compile one or more components~n"
              "shell <component>...    start a shell running one or more "
              "components~n"
              "test <component>...     test one or more components~n"
              "help                    print help and exit~n">>,
            [ProgramName]).

-spec load_manifest(command_line_options()) -> eon_manifest:manifest().
load_manifest(Options) ->
  Root = maps:get(root, Options, <<".">>),
  Path = filename:join(Root, "eon.erl"),
  eon_manifest:load(Path).

-spec cmd_build(command_line_data(), eon_manifest:manifest()) -> ok.
cmd_build(CommandLineData, Manifest) ->
  Build =
    fun (Component) ->
        eon_log:info("building component ~ts", [Component]),
        eon_manifest:compile(Component, Manifest),
        ArtifactPath = eon_manifest:build(Component, Manifest),
        eon_log:info("component built at ~ts", [ArtifactPath])
    end,
  apply_component_command(Build, CommandLineData, Manifest).

-spec cmd_compile(command_line_data(), eon_manifest:manifest()) -> ok.
cmd_compile(CommandLineData, Manifest) ->
  Compile =
    fun (Component) ->
        eon_log:info("compiling component ~ts", [Component]),
        eon_manifest:compile(Component, Manifest)
    end,
  apply_component_command(Compile, CommandLineData, Manifest).

-spec cmd_shell(command_line_data(), eon_manifest:manifest()) -> no_return().
cmd_shell(CommandLineData, Manifest) ->
  cmd_compile(CommandLineData, Manifest),
  Components = command_line_components(CommandLineData, Manifest),
  CodePaths = eon_manifest:code_paths(Components, Manifest),
  code:add_paths([eon_fs:path_string(Path) || Path <- CodePaths]),
  Apps = eon_manifest:applications(Components, Manifest),
  start_shell_applications(Apps),
  eon_log:info("starting shell"),
  case shell:start_interactive() of
    ok ->
      run_shell();
    {error, Reason} ->
      throw({error, {start_shell, Reason}})
  end.

-spec cmd_test(command_line_data(), eon_manifest:manifest()) -> ok.
cmd_test(CommandLineData = #{options := Options}, Manifest) ->
  Compile =
    fun (Component) ->
        eon_log:info("testing component ~ts", [Component]),
        eon_manifest:compile(Component, Manifest),
        TestCfg = #{component => Component,
                    verbose => maps:get(verbose, Options, false)},
        eon_manifest:test(Component, Manifest, TestCfg)
    end,
  case apply_component_command(Compile, CommandLineData, Manifest) of
    ok -> ok;
    error -> halt(1)
  end.

-spec start_shell_applications([atom()]) -> ok.
start_shell_applications([]) ->
  ok;
start_shell_applications([App | Apps]) ->
  eon_log:info("starting application ~p", [App]),
  case application:ensure_all_started(App, temporary) of
    {ok, _} ->
      start_shell_applications(Apps);
    {error, Reason} ->
      throw({error, {start_application, App, Reason}})
  end.

-spec run_shell() -> no_return().
run_shell() ->
  receive
    Msg ->
      %% Are there messages we should process?
      eon_log:error("unhandled message ~tp", [Msg]),
      run_shell()
  end.

-spec command_line_components(command_line_data(), eon_manifest:manifest()) ->
        [atom()].
command_line_components(#{arguments := []}, Manifest) ->
  eon_manifest:component_names(Manifest);
command_line_components(#{arguments := Arguments}, _Manifest) ->
  [erlang:binary_to_atom(Name) || Name <- Arguments].

-spec apply_component_command(fun((atom()) -> ok | error), command_line_data(),
                              eon_manifest:manifest()) -> ok | error.
apply_component_command(Fun, CommandLineData, Manifest) ->
  Components = command_line_components(CommandLineData, Manifest),
  lists:foldl(fun (Component, Result) ->
                  case Fun(Component) of
                    ok -> Result;
                    error -> error
                  end
              end, ok, Components).

-spec parse_command_line([string()]) -> command_line_data().
parse_command_line(Args) ->
  RawArgs = [eon_string:binary(Arg) || Arg <- Args],
  Acc = #{arguments => [], options => #{}},
  parse_command_line(RawArgs, options, Acc).

-spec parse_command_line([binary()], State, map()) -> command_line_data() when
    State :: options | arguments.
parse_command_line([], _State, #{command := Command,
                                 arguments := Arguments,
                                 options := Options}) ->
  #{command => Command,
    arguments => lists:reverse(Arguments),
    options => Options};
parse_command_line([], options, Acc = #{options := #{help := true}}) ->
  parse_command_line([], options, Acc#{command => help});
parse_command_line([], options, _Acc) ->
  throw({error, missing_command});
parse_command_line([<<$-, Option>> | RawArgs], options,
                   Acc = #{options := Options}) ->
  {Options2, RawArgs3} =
    case {Option, RawArgs} of
      {$C, []} ->
        throw({error, {missing_option_value, $C}});
      {$C, [Path | RawArgs2]} ->
        {Options#{root => Path}, RawArgs2};
      {$h, _} ->
        {Options#{help => true}, RawArgs};
      {$v, _} ->
        {Options#{verbose => true}, RawArgs};
      _ ->
        throw({error, {unknown_option, [$-, Option]}})
    end,
  parse_command_line(RawArgs3, options, Acc#{options => Options2});
parse_command_line([Option = <<$-, _/binary>> | _], options, _Acc) ->
  throw({error, {invalid_long_option, Option}});
parse_command_line(RawArgs, options, Acc = #{command := _}) ->
  parse_command_line(RawArgs, arguments, Acc);
parse_command_line([Argument | RawArgs], arguments,
                   Acc = #{arguments := Arguments}) ->
  parse_command_line(RawArgs, arguments,
                     Acc#{arguments => [Argument | Arguments]});
parse_command_line([Name | RawArgs], options, Acc) ->
  Command =
    case Name of
      <<"build">> -> build;
      <<"compile">> -> compile;
      <<"help">> -> help;
      <<"shell">> -> shell;
      <<"test">> -> test;
      _ ->
        throw({error, {unhandled_command, Name}})
    end,
  parse_command_line(RawArgs, options, Acc#{command => Command}).
