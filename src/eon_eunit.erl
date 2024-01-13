-module(eon_eunit).

-export([test/2]).

-spec test(Module :: atom(), eon_manifest:manifest()) -> ok.
test(Module, _Manifest) ->
  eon_log:info("testing ~ts", [Module]),
  Options = [{print_depth, 20}],
  case eunit:test(Module, Options) of
    ok ->
      ok;
    error ->
      %% This one is not described by the test specification of eunit:test/2
      throw({error, {eunit, test_failure}});
    {error, Reason} ->
      throw({error, {eunit, Reason}})
  end.
