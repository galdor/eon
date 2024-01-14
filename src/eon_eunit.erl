-module(eon_eunit).

-export([test/3]).

-spec test(Modules :: [atom()], eon_manifest:manifest(), eon:test_cfg()) ->
        ok | error.
test(Modules, _Manifest, TestCfg) ->
  eon_log:info("executing eunit tests"),
  ReporterCfg = [{eon_test_cfg, TestCfg}],
  Options = [{print_depth, 20},
             {report, {eon_eunit_reporter, ReporterCfg}},
             no_tty],
  case eunit:test(Modules, Options) of
    ok ->
      ok;
    error ->
      error;
    {error, Reason} ->
      throw({error, {eunit, Reason}})
  end.
