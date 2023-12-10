#{project =>
    #{name => "hello",
      description => "A minimal Erlang application demonstration."},
  dependencies =>
    #{},
  components =>
    #{hello =>
        #{type => escript,
          name => "hello",
          main_module => hello,
          applications =>
            [hello]}}}.
