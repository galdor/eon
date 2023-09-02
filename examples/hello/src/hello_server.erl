-module(hello_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  schedule_hello(),
  {ok, undefined}.

terminate(_Reason, _State) ->
  ok.

handle_call(_Msg, _From, State) ->
  {reply, unhandled, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({hello, Message}, State) ->
  io:format("~ts~n", [Message]),
  schedule_hello(),
  {noreply, State}.

schedule_hello() ->
  erlang:send_after(1000, self(), {hello, <<"Hello world!">>}).
