-module(hello).

-export([main/1]).

main(_Args) ->
  case application:start(hello) of
    ok ->
      ok;
    {error, Reason} ->
      io:format(standard_error, "cannot start application ~tp: ~tp",
                [hello, Reason]),
      halt(1)
  end.
