-module(eon_eunit_reporter).

-behaviour(eunit_listener).

-export([start/1]).
-export([init/1, handle_begin/3, handle_end/3, handle_cancel/3, terminate/2]).

-type options() :: [option()].

-type option() ::
        {spawn, [erlang:spawn_opt_option()]} % handled by eunit_listener:start/2
      | {eon_test_cfg, eon:test_cfg()}.

-type state() ::
        #{cfg := eon:test_cfg(),
          test_failures := [test_result()]}.

-type id() :: [pos_integer()].
-type status() :: ok | {error, _}.

-type test_result() ::
        #{id := id(),
          description => binary(),
          source => mfa(),
          line => pos_integer(),
          status := status(),
          time => non_neg_integer(),
          output => binary()}.

%% Expected by eunit (see eunit:start_listeners/1)
-spec start(options()) -> pid() | {pid(), reference()}.
start(Options) ->
  eunit_listener:start(?MODULE, Options).

-spec init(options()) -> state().
init(Options) ->
  #{cfg => proplists:get_value(eon_test_cfg, Options),
    test_failures => []}.

-spec handle_begin(group | test, list(), state()) -> state().
handle_begin(group, Data, State) ->
  format_group_start(Data, State),
  State;
handle_begin(test, _Data, State) ->
  State.

-spec handle_end(group | test, list(), state()) -> state().
handle_end(group, Data, State) ->
  format_group_end(ended, Data, State),
  State;
handle_end(test, Data, State = #{test_failures := TestFailures}) ->
  Result = init_test_result(Data),
  format_test_end(ended, Result, State),
  case maps:get(status, Result) of
    ok ->
      State;
    {error, _} ->
      State#{test_failures => [Result | TestFailures]}
  end.

-spec handle_cancel(group | test, list(), state()) -> state().
handle_cancel(group, Data, State) ->
  format_group_end(cancelled, Data, State),
  State;
handle_cancel(test, Data, State) ->
  Result = init_test_result(Data),
  format_test_end(cancelled, Result, State),
  State.

-spec terminate({ok, _} | {error, _}, state()) -> ok.
terminate({ok, Data}, _State = #{cfg := #{component := Component},
                                 test_failures := TestFailures}) ->
  format_test_failures(lists:reverse(TestFailures)),
  Passed = proplists:get_value(pass, Data, 0),
  Failed = proplists:get_value(fail, Data, 0),
  Skipped = proplists:get_value(skip, Data, 0),
  Cancelled = proplists:get_value(cancel, Data, 0),
  Total = Passed + Failed + Skipped + Cancelled,
  case {Passed, Total} of
    {_, 0} ->
      io:format("~ts: no test found~n", [Component]);
    {Total, Total} ->
      io:format("~ts: ~b/~b tests passed~n", [Component, Passed, Total]);
    _ ->
      io:format("~ts: ~b/~b tests passed"
                " (~b failed, ~b skipped, ~b cancelled)~n",
                [Component, Passed, Total, Failed, Skipped, Cancelled])
  end;
terminate({error, {throw, Reason, Trace}}, _State) ->
  eon_log:error("tests failed with exception ~tp~n  ~tp~n", [Reason, Trace]);
terminate({error, {error, Reason, Trace}}, _State) ->
  eon_log:error("tests failed with error ~tp~n  ~tp~n", [Reason, Trace]);
terminate({error, {exit, Reason, Trace}}, _State) ->
  eon_log:error("tests exited with error ~tp~n  ~tp~n", [Reason, Trace]).

-spec format_group_start(list(), state()) -> ok.
format_group_start(Data, #{cfg := #{verbose := true}}) ->
  Id = proplists:get_value(id, Data),
  Indentation = indentation(Id),
  Source = proplists:get_value(source, Data),
  Line = proplists:get_value(line, Data),
  Description= proplists:get_value(desc, Data),
  Label = format_label(group, Source, Line, Description),
  io:format("  ~-70ts~n", [[Indentation, Label]]);
format_group_start(_Data, _State) ->
  ok.

-spec format_group_end(ended | cancelled, list(), state()) -> ok.
format_group_end(Event, Data, #{cfg := #{verbose := true}}) ->
  Id = proplists:get_value(id, Data),
  Indentation = indentation(Id),
  Source = proplists:get_value(source, Data),
  Line = proplists:get_value(line, Data),
  Description= proplists:get_value(desc, Data),
  Label = format_label(group, Source, Line, Description),
  case Event of
    ended ->
      Time = format_time(proplists:get_value(time, Data, 0)),
      io:format("  ~-70ts ~ts~n", [[Indentation, Label], Time]);
    cancelled ->
      io:format("! ~ts: cancelled~n", [[Indentation, Label]])
  end;
format_group_end(_Event, _Data, _State) ->
  ok.

-spec format_test_end(ended | cancelled, test_result(), state()) -> ok.
format_test_end(Event, Result, #{cfg := #{verbose := true}}) ->
  Indentation = test_result_indentation(Result),
  Label = format_test_result_label(Result),
  Mark = case {Event, maps:get(status, Result, undefined)} of
           {ended, ok} -> " ";
           _ -> "!"
         end,
  case Event of
    ended ->
      Time = format_time(maps:get(time, Result, 0)),
      io:format("~ts ~-70ts ~ts~n", [Mark, [Indentation, Label], Time]);
    cancelled ->
      io:format("! ~ts: cancelled~n", [[Indentation, Label]])
  end;
format_test_end(_Event, _Data, _State) ->
  ok.

-spec format_test_failures([test_result()]) -> ok.
format_test_failures(Results) ->
  lists:foreach(fun format_test_failure/1, Results).

-spec format_test_failure(test_result()) -> ok.
format_test_failure(Result = #{status := {error, Error}}) ->
  io:format("~n~ts~n", [format_test_result_source(Result)]),
  case maps:find(description, Result) of
    {ok, Description} -> io:format("  description: ~ts~n", [Description]);
    error -> ok
  end,
  %% TODO format error
  io:format("  ~tp~n", [Error]),
  case maps:find(output, Result) of
    {ok, Output} ->
      io:format("  output:~n"),
      Lines = binary:split(Output, <<"\n">>, [global, trim]),
      lists:foreach(fun (Line) ->
                        io:format("    ~ts~n", [Line])
                    end, Lines);
    error ->
      ok
  end.

-spec init_test_result(list()) -> test_result().
init_test_result(Data) ->
  Id = proplists:get_value(id, Data),
  Id =:= undefined
    andalso throw({error, {missing_test_id, Data}}),
  Status = proplists:get_value(status, Data),
  Status =:= undefined
    andalso throw({error, {missing_test_status, Data}}),
  init_test_result(Data, #{id => Id, status => Status}).

-spec init_test_result(list(), test_result()) -> test_result().
init_test_result(Data, Result0) ->
  lists:foldl(fun
                ({desc, D}, Result) when is_list(D); is_binary(D) ->
                 Result#{description => eon_string:binary(D)};
                ({source, Source}, Result) ->
                 Result#{source => Source};
                ({line, Line}, Result) ->
                 Result#{line => Line};
                ({status, Status}, Result) ->
                 Result#{status => Status};
                ({time, Time}, Result) ->
                 Result#{time => Time};
                ({output, O}, Result) when is_list(O) ->
                 case iolist_size(O) of
                   0 -> Result;
                   _ -> Result#{output => eon_string:binary(O)}
                 end;
                ({output, O}, Result) when is_binary(O), byte_size(O) > 0 ->
                 Result#{output => O};
                (_Entry, Result) ->
                 Result
             end, Result0, Data).

-spec indentation(id()) -> unicode:chardata().
indentation(Ids) ->
  ["  " || _ <- Ids].

-spec test_result_indentation(test_result()) -> unicode:chardata().
test_result_indentation(#{id := Id}) ->
  indentation(Id).

-spec format_test_result_source(test_result()) -> io_lib:chars().
format_test_result_source(Result) ->
  Source = maps:get(source, Result, undefined),
  Line = maps:get(line, Result, undefined),
  format_test_source(Source, Line).

-spec format_test_result_label(test_result()) -> io_lib:chars().
format_test_result_label(Result) ->
  Source = maps:get(source, Result, undefined),
  Line = maps:get(line, Result, undefined),
  Description = maps:get(description, Result, undefined),
  format_label(test, Source, Line, Description).

-spec format_label(group | test, Source, Line, Description) -> io_lib:chars() when
    Source :: mfa() | undefined,
    Line :: pos_integer() | undefined,
    Description :: binary() | string() | undefined.
format_label(group, undefined, _Line, undefined) ->
  "group";
format_label(test, undefined, _Line, undefined) ->
  "test";
format_label(_Type, Source, Line, undefined) ->
  format_test_source(Source, Line);
format_label(_Type, _Source, _Line, Description) ->
  Description.

-spec format_test_source(Source, Line) -> io_lib:chars() when
    Source :: mfa() | undefined,
    Line :: pos_integer() | undefined.
format_test_source({M, F, _A}, undefined) ->
  io_lib:format("~ts:~ts", [M, F]);
format_test_source({M, F, _A}, Line) ->
  io_lib:format("~ts:~ts:~b", [M, F, Line]).

-spec format_time(non_neg_integer()) -> io_lib:chars().
format_time(0) ->
  "      ";
format_time(T) when T < 1000 ->
  io_lib:format("~4bms", [T]);
format_time(T) ->
  io_lib:format("~2.1fs", [T/1000]).
