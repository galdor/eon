#!/usr/bin/env escript
%%! -pa ebin

main([]) ->
  process_flag(trap_exit, true),
  eon_log:start(#{debug_level => 1}),
  {ok, Manifest} = eon_manifest:load("eon.erl"),
  case eon_manifest:build(eon, Manifest) of
    {ok, ArtifactPath, Warnings} ->
      lists:foreach(fun (Warning) ->
                        eon_log:info("warning: ~tp", [Warning])
                    end, Warnings),
      eon_log:info("component built at ~ts", [ArtifactPath]);
    {error, Reason} ->
      eon_log:fatal("cannot build component ~ts: ~tp", [eon, Reason])
  end,
  eon_log:stop().
