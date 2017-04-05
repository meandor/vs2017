-module(hbq_tests).
-include_lib("eunit/include/eunit.hrl").

full_test_() ->
  {foreach, fun setup/0, fun teardown/1,
    [{"Assert X", fun initHBQ_test/0},
      {"Assert X", fun test_test/0}]}.

setup() -> register(holdbackq, spawn_link(hbq, initHBQ, [14, "HBQTestLogging"])).
teardown(ArgFromSetup) -> ok.

% Should create an empty DLQ
initHBQ_test() ->
  holdbackq ! dellHBQ, ok.

test_test() ->
  holdbackq ! {pushHBQ , [1, "bla", erlang:now(), erlang:now()],  "HBQTestLogging"},
  holdbackq ! {pushHBQ , [2, "bla", erlang:now(), erlang:now()],  "HBQTestLogging"},
  holdbackq ! {pushHBQ , [3, "bla", erlang:now(), erlang:now()],  "HBQTestLogging"},
  holdbackq ! {pushHBQ , [4, "bla", erlang:now(), erlang:now()],  "HBQTestLogging"}.
