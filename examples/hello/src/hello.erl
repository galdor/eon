-module(hello).

-export([main/1]).

main(_Args) ->
  case application:start(hello) of
    ok ->
      wait();
    {error, Reason} ->
      io:format(standard_error, "cannot start application ~tp: ~0tp~n",
                [hello, Reason]),
      halt(1)
  end.

wait() ->
  timer:sleep(1000),
  wait().
