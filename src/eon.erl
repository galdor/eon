-module(eon).

-export([main/1]).

-type command_line_error_reason() ::
        missing_command
      | {missing_option_value, char()}
      | {unknown_option, char()}
      | {invalid_long_option, binary()}
      | {unknown_command, binary()}.

-type command_line_data() ::
        #{command := atom(),
          arguments := [binary()],
          options := command_line_options()}.

-type command_line_options() ::
        #{atom() := binary()}.

main(Args) ->
  process_flag(trap_exit, true),
  eon_log:start(#{debug_level => 1}),
  case parse_command_line(Args) of
    {ok, #{command := help}} ->
      usage();
    {ok, #{options := #{help := true}}} ->
      usage();
    {ok, CommandLineData = #{command := compile}} ->
      cmd_compile(CommandLineData);
    {ok, CommandLineData = #{command := build}} ->
      cmd_build(CommandLineData);
    {error, Reason} ->
      eon_log:fatal("invalid command line arguments: ~tp", [Reason])
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
  case eon_manifest:load(Path) of
    {ok, Manifest} ->
      Manifest;
    {error, Reason} ->
      eon_log:fatal("cannot load manifest from ~ts: ~tp", [Path, Reason])
  end.

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
  case eon_manifest:build(ComponentName, Manifest) of
    {ok, ArtifactPath} ->
      eon_log:info("component built at ~ts", [ArtifactPath]),
      build(ComponentNames, Manifest, Options);
    {error, Reason} ->
      eon_log:fatal("cannot build component ~ts: ~tp",
                    [ComponentName, Reason])
  end.

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
  case eon_manifest:compile(ComponentName, Manifest) of
    {ok, Diagnostics} ->
      lists:foreach(fun (Diagnostic) ->
                        eon_log:info("warning: ~tp", [Diagnostic])
                    end, Diagnostics),
      eon_log:info("component compiled"),
      compile(ComponentNames, Manifest, Options);
    {error, Reason} ->
      eon_log:fatal("cannot compile component ~ts: ~tp",
                    [ComponentName, Reason])
  end.

-spec parse_command_line([string()]) ->
        {ok, command_line_data()} | {error, command_line_error_reason()}.
parse_command_line(Args) ->
  RawArgs = [eon_string:binary(Arg) || Arg <- Args],
  Acc = #{arguments => [], options => #{}},
  parse_command_line(RawArgs, options, Acc).

-spec parse_command_line([binary()], State, map()) ->
        {ok, command_line_data()} | {error, command_line_error_reason()} when
    State :: options | arguments.
parse_command_line([], _State, #{command := Command,
                                 arguments := Arguments,
                                 options := Options}) ->
  {ok, #{command => Command,
         arguments => lists:reverse(Arguments),
         options => Options}};
parse_command_line([], options, Acc = #{options := #{help := true}}) ->
  parse_command_line([], options, Acc#{command => help});
parse_command_line([], options, _Acc) ->
  {error, missing_command};
parse_command_line([<<$-, Option>> | RawArgs], options,
                   Acc = #{options := Options}) ->
  case
    case {Option, RawArgs} of
      {$C, []} ->
        {error, {missing_option_value, $C}};
      {$C, [Path | RawArgs2]} ->
        {ok, Options#{root => Path}, RawArgs2};
      {$h, _} ->
        {ok, Options#{help => true}, RawArgs};
      _ ->
        error
    end
  of
    {ok, Options2, RawArgs3} ->
      parse_command_line(RawArgs3, options, Acc#{options => Options2});
    {error, Reason} ->
      {error, Reason};
    error ->
      {error, {unknown_option, Option}}
  end;
parse_command_line([Option = <<$-, _/binary>> | _], options, _Acc) ->
  {error, {invalid_long_option, Option}};
parse_command_line(RawArgs, options, Acc = #{command := _}) ->
  parse_command_line(RawArgs, arguments, Acc);
parse_command_line([Argument | RawArgs], arguments,
                   Acc = #{arguments := Arguments}) ->
  parse_command_line(RawArgs, arguments,
                     Acc#{arguments => [Argument | Arguments]});
parse_command_line([Name | RawArgs], options, Acc) ->
  case
    case Name of
      <<"build">> ->
        {ok, build};
      <<"compile">> ->
        {ok, compile};
      <<"help">> ->
        {ok, help};
      _ ->
        error
    end
  of
    {ok, Command} ->
      parse_command_line(RawArgs, options, Acc#{command => Command});
    error ->
      {error, {unknown_command, Name}}
  end.
