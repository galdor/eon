#!/usr/bin/env escript
%%! -pa ebin

main([]) ->
  process_flag(trap_exit, true),
  eon_log:start(#{debug_level => 1}),
  Manifest = eon_manifest:load("eon.erl"),
  Diagnostics =  eon_manifest:compile(eon, Manifest),
  lists:foreach(fun (Diagnostic) ->
                    eon_log:info("warning: ~tp", [Diagnostic])
                end, Diagnostics),
  ArtifactPath = eon_manifest:build(eon, Manifest),
  eon_log:info("component built at ~ts", [ArtifactPath]),
  eon_log:stop().
