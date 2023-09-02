-module(hello_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link(?MODULE, []).

init(_Args) ->
  Children = [#{id => hello_server,
                start => {hello_server, start_link, []}}],
  {ok, {#{}, Children}}.
