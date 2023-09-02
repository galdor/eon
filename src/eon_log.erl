-module(eon_log).

-export([start/1, stop/0,
         log/3, debug/2, debug/3, info/1, info/2, error/1, error/2,
         fatal/1, fatal/2,
         init/1]).

-export_type([level/0, cfg/0]).

-type level() ::
        {debug, pos_integer()}
      | info
      | error
      | fatal.

-type cfg() ::
        #{debug_level => non_neg_integer()}.

-spec start(cfg()) -> ok.
start(Cfg) ->
  Pid = erlang:spawn_link(?MODULE, init, [Cfg]),
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
  ?MODULE ! {log, Level, Format, Args}.

-spec debug(pos_integer(), io:format()) -> ok.
debug(DebugLevel, Format) ->
  debug(DebugLevel, Format, []).

-spec debug(pos_integer(), io:format(), [term()]) -> ok.
debug(DebugLevel, Format, Args) ->
  log({debug, DebugLevel}, Format, Args).

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

-spec fatal(io:format()) -> ok.
fatal(Format) ->
  ?MODULE:fatal(Format, []).

-spec fatal(io:format(), [term()]) -> ok.
fatal(Format, Args) ->
  log(fatal, Format, Args).

-spec init(cfg()) -> no_return().
init(Cfg) ->
  main(Cfg).

-spec main(cfg()) -> no_return().
main(Cfg) ->
  receive
    {log, Level, Message, Args} ->
      do_log(Level, Message, Args, Cfg),
      main(Cfg);
    stop ->
      exit(normal);
    Msg ->
      do_log(error, "unhandled message ~tp", [Msg], Cfg),
      main(Cfg)
  end.

-spec do_log(level(), io:format(), [term()], cfg()) -> ok.
do_log({debug, DebugLevel}, Format, Args, Cfg) ->
  case maps:get(debug_level, Cfg, 0) of
    Level when Level >= DebugLevel ->
      io:format(standard_error, Format, Args),
      io:nl(standard_error);
    _ ->
      ok
  end;
do_log(info, Format, Args, _Cfg) ->
  io:format(standard_error, Format, Args),
  io:nl(standard_error);
do_log(error, Format, Args, _Cfg) ->
  io:format(standard_error, "error: ", []),
  io:format(standard_error, Format, Args),
  io:nl(standard_error);
do_log(fatal, Format, Args, _Cfg) ->
  io:format(standard_error, "fatal error: ", []),
  io:format(standard_error, Format, Args),
  io:nl(standard_error),
  halt(1).
