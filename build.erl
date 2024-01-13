#!/usr/bin/env escript
%%! -pa ebin

main([]) ->
  process_flag(trap_exit, true),
  eon_log:start(#{level => debug}),
  Manifest = eon_manifest:load("eon.erl"),
  eon_manifest:compile(eon, Manifest),
  ArtifactPath = eon_manifest:build(eon, Manifest),
  eon_log:info("component built at ~ts", [ArtifactPath]),
  eon_log:stop().
