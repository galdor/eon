-module(eon).

-export([main/1]).

-type command_line_data() ::
        #{command := atom(),
          arguments := [binary()],
          options := command_line_options()}.

-type command_line_options() ::
        #{atom() := boolean() | binary()}.

main(Args) ->
  process_flag(trap_exit, true),
  eon_log:start(#{debug_level => 1}),
  case parse_command_line(Args) of
    #{command := help} ->
      usage();
    #{options := #{help := true}} ->
      usage();
    CommandLineData = #{command := compile} ->
      cmd_compile(CommandLineData);
    CommandLineData = #{command := build} ->
      cmd_build(CommandLineData)
  end,
  eon_log:stop().

usage() ->
  ProgramName = escript:script_name(),
  io:format(<<"Usage: ~s OPTIONS <command>~n"
              "~n"
              "OPTIONS~n"
              "~n"
              "-C <path>               set the root directory of the project"
              " (default: .)~n"
              "-h                      print help and exit~n"
              "~n"
              "COMMANDS~n"
              "~n"
              "build                   build all components~n"
              "build <component>...    build one or more components~n"
              "compile <component>...  compile one or more components~n"
              "help                    print help and exit~n">>,
            [ProgramName]).

-spec load_manifest(command_line_options()) -> eon_manifest:manifest().
load_manifest(Options) ->
  Root = maps:get(root, Options, <<".">>),
  Path = filename:join(Root, "eon.erl"),
  eon_manifest:load(Path).

-spec cmd_build(command_line_data()) -> ok.
cmd_build(#{options := Options, arguments := Args}) ->
  Manifest = load_manifest(Options),
  Components = case Args of
                 [] ->    eon_manifest:component_names(Manifest);
                 Names -> [erlang:binary_to_atom(Name) || Name <- Names]
               end,
  compile(Components, Manifest, #{}),
  build(Components, Manifest, Options).

-spec build(ComponentNames, eon_manifest:manifest(),
            command_line_options()) -> ok when
    ComponentNames :: [atom()].
build([], _Manifest, _Options) ->
  ok;
build([ComponentName | ComponentNames], Manifest, Options) ->
  eon_log:info("building component ~ts", [ComponentName]),
  ArtifactPath = eon_manifest:build(ComponentName, Manifest),
  eon_log:info("component built at ~ts", [ArtifactPath]),
  build(ComponentNames, Manifest, Options).

-spec cmd_compile(command_line_data()) -> ok.
cmd_compile(#{options := Options, arguments := Args}) ->
  Manifest = load_manifest(Options),
  Components = case Args of
                 [] ->    eon_manifest:component_names(Manifest);
                 Names -> [erlang:binary_to_atom(Name) || Name <- Names]
               end,
  compile(Components, Manifest, Options).

-spec compile(ComponentNames, eon_manifest:manifest(),
              command_line_options()) -> ok when
    ComponentNames :: [atom()].
compile([], _Manifest, _Options) ->
  ok;
compile([ComponentName | ComponentNames], Manifest, Options) ->
  Diagnostics = eon_manifest:compile(ComponentName, Manifest),
  lists:foreach(fun (Diagnostic) ->
                    eon_log:info("warning: ~tp", [Diagnostic])
                end, Diagnostics),
  eon_log:info("component compiled"),
  compile(ComponentNames, Manifest, Options).

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
      _ -> error
    end,
  parse_command_line(RawArgs, options, Acc#{command => Command}).
