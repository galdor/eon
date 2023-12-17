-module(eon_system).

-export([exec/2, exec/3]).

-type exec_options() ::
   #{first_line => boolean()}.

-spec exec(file:name_all(), [string()]) -> binary().
exec(ProgramName, Arguments) ->
  exec(ProgramName, Arguments, #{}).

-spec exec(file:name_all(), [string()], exec_options()) -> binary().
exec(ProgramName, Arguments, Options) ->
  case os:find_executable(ProgramName) of
    ProgramPath when is_list(ProgramPath) ->
      exec_program(ProgramPath, Arguments, Options);
    false ->
      throw({error, {program_not_found, ProgramName}})
  end.

-spec exec_program(string(), [string()], exec_options()) -> binary().
exec_program(ProgramPath, Arguments, Options) ->
  PortOptions = [{args, Arguments},
                 use_stdio,
                 stderr_to_stdout,
                 binary,
                 exit_status],
  Port = open_port({spawn_executable, ProgramPath}, PortOptions),
  watch_program(Port, Options, []).

-spec watch_program(port(), exec_options(), iodata()) -> binary().
watch_program(Port, Options, Output) ->
  receive
    {Port, {data, Data}} ->
      watch_program(Port, Options, [Data | Output]);
    {Port, {exit_status, 0}} ->
      watch_program(Port, Options, Output);
    {Port, {exit_status, Code}} when Code < 128 ->
      ErrorOutput = eon_string:binary(lists:reverse(Output)),
      throw({error, {program_failure, Code, ErrorOutput}});
    {Port, {exit_status, Code}} ->
      Signal = Code - 128,
      ErrorOutput = eon_string:binary(lists:reverse(Output)),
      throw({error, {program_killed, Signal, ErrorOutput}});
    {'EXIT', Port, _} ->
      Output2 = eon_string:binary(lists:reverse(Output)),
      case maps:get(first_line, Options, false) of
        true ->
          case binary:match(Output2, <<$\n>>) of
            {Position, _} ->
              binary:part(Output2, {0, Position});
            nomatch ->
              Output2
          end;
        false ->
          Output2
      end
  end.
