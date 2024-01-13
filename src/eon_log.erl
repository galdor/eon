-module(eon_log).

-export([start/1, stop/0,
         log/3, debug/1, debug/2, info/1, info/2, error/1, error/2,
         init/2]).

-export_type([level/0, cfg/0]).

-type level() ::
        debug
      | info
      | error.

-type cfg() ::
        #{level => level()}.

-type state() ::
        #{level_rank := pos_integer()}.

-spec start(cfg()) -> ok.
start(Cfg) ->
  Level = maps:get(level, Cfg, info),
  State = #{level_rank => level_rank(Level)},
  Pid = erlang:spawn_link(?MODULE, init, [Cfg, State]),
  erlang:register(?MODULE, Pid),
  ok.

-spec stop() -> ok.
stop() ->
  case whereis(?MODULE) of
    undefined ->
      erlang:error("~tp is not running", [?MODULE]);
    Pid ->
      Pid ! stop,
      receive
        {'EXIT', Pid, _Reason} ->
          ok
      end
  end.

-spec log(level(), io:format(), [term()]) -> ok.
log(Level, Format, Args) ->
  ?MODULE ! {log, Level, Format, Args},
  ok.

-spec debug(io:format()) -> ok.
debug(Format) ->
  debug(Format, []).

-spec debug(io:format(), [term()]) -> ok.
debug(Format, Args) ->
  log(debug, Format, Args).

-spec info(io:format()) -> ok.
info(Format) ->
  info(Format, []).

-spec info(io:format(), [term()]) -> ok.
info(Format, Args) ->
  log(info, Format, Args).

-spec error(io:format()) -> ok.
error(Format) ->
  ?MODULE:error(Format, []).

-spec error(io:format(), [term()]) -> ok.
error(Format, Args) ->
  log(error, Format, Args).

-spec init(cfg(), state()) -> no_return().
init(Cfg, State) ->
  main(Cfg, State).

-spec main(cfg(), state()) -> no_return().
main(Cfg, State = #{level_rank := LevelRank}) ->
  receive
    {log, Level, Message, Args} ->
      case level_rank(Level) of
        MessageLevelRank when MessageLevelRank >= LevelRank ->
          do_log(Level, Message, Args, Cfg);
        _ ->
          ok
      end,
      main(Cfg, State);
    stop ->
      exit(normal);
    Msg ->
      do_log(error, "unhandled message ~tp", [Msg], Cfg),
      main(Cfg, State)
  end.

-spec do_log(level(), io:format(), [term()], cfg()) -> ok.
do_log(debug, Format, Args, _Cfg) ->
  Format2 = eon_string:binary([Format, "~n"]),
  io:format(standard_error, Format2, Args);
do_log(info, Format, Args, _Cfg) ->
  Format2 = eon_string:binary([Format, "~n"]),
  io:format(standard_error, Format2, Args);
do_log(error, Format, Args, _Cfg) ->
  Format2 = eon_string:binary(["error: ", Format, "~n"]),
  io:format(standard_error, Format2, Args).

-spec level_rank(level()) -> pos_integer().
level_rank(debug) -> 1;
level_rank(info) -> 2;
level_rank(error) -> 3.
