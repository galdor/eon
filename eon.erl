#{project =>
    #{name => "eon",
      description => "A simple Erlang build tool."},
  components =>
    #{eon =>
        #{type => escript,
          name => "eon",
          main_module => eon,
          applications =>
            [eon]}}}.
