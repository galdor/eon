-module(eon_system).

-export([exec/2, exec/3]).

-export_type([exec_error_reason/0]).

-type exec_error_reason() ::
        program_not_found
      | {program_failure, Status :: integer(), iodata()}
      | {program_killed, Signal :: integer(), iodata()}.

-type exec_options() ::
   #{first_line => boolean()}.

-spec exec(file:name_all(), [string()]) ->
        {ok, binary()} | {error, exec_error_reason()}.
exec(ProgramName, Arguments) ->
  exec(ProgramName, Arguments, #{}).

-spec exec(file:name_all(), [string()], exec_options()) ->
        {ok, binary()} | {error, exec_error_reason()}.
exec(ProgramName, Arguments, Options) ->
  case os:find_executable(ProgramName) of
    ProgramPath when is_list(ProgramPath) ->
      exec_program(ProgramPath, Arguments, Options);
    false ->
      {error, program_not_found}
  end.

-spec exec_program(string(), [string()], exec_options()) ->
        {ok, binary()} | {error, exec_error_reason()}.
exec_program(ProgramPath, Arguments, Options) ->
  PortOptions = [{args, Arguments},
                 use_stdio,
                 stderr_to_stdout,
                 binary,
                 exit_status],
  Port = open_port({spawn_executable, ProgramPath}, PortOptions),
  watch_program(Port, Options, [], undefined).

-spec watch_program(port(), exec_options(), iodata(), term()) ->
        {ok, binary()} | {error, exec_error_reason()}.
watch_program(Port, Options, Output, Error) ->
  receive
    {Port, {data, Data}} ->
      watch_program(Port, Options, [Data | Output], Error);
    {Port, {exit_status, 0}} ->
      watch_program(Port, Options, Output, Error);
    {Port, {exit_status, Code}} when Code < 128 ->
      ErrorOutput = eon_string:binary(lists:reverse(Output)),
      watch_program(Port, Options, Output,
                    {program_failure, Code, ErrorOutput});
    {Port, {exit_status, Code}} ->
      Signal = Code - 128,
      ErrorOutput = eon_string:binary(lists:reverse(Output)),
      watch_program(Port, Options, Output,
                    {program_killed, Signal, ErrorOutput});
    {'EXIT', Port, _} ->
      case Error of
        undefined ->
          Output2 = eon_string:binary(lists:reverse(Output)),
          case maps:get(first_line, Options, false) of
            true ->
              case binary:match(Output2, <<$\n>>) of
                {Position, _} ->
                  {ok, binary:part(Output2, {0, Position})};
                nomatch ->
                  {ok, Output2}
              end;
            false ->
              {ok, Output2}
          end;
        _ ->
          {error, Error}
      end
  end.
