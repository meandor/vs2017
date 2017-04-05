-module(hbq_tests).
-include_lib("eunit/include/eunit.hrl").

%full_test_() ->
%  {foreach, fun setup/0, fun teardown/1,
%    [{"Assert X", fun initHBQ_test/0},
%      {"Assert X", fun test_test/0}]}.

setup() -> ok.
teardown(ArgFromSetup) -> ok.

%initHBQ_test() ->
%  PID = spawn(hbq, initHBQ, [2,"HBQTestLogging.log"]),
%  PID ! dellHBQ, ok.

%test_test() ->
%  PID = spawn(hbq, initHBQ, [ 2,"HBQTestLogging.log"]),
%  PID ! {pushHBQ , [1, "bla", erlang:now(), erlang:now()],  "HBQTestLogging.log"},
%  PID ! {pushHBQ , [2, "bla", erlang:now(), erlang:now()],  "HBQTestLogging.log"},
%  PID ! {pushHBQ , [3, "bla", erlang:now(), erlang:now()],  "HBQTestLogging.log"},
%  PID ! {pushHBQ , [4, "bla", erlang:now(), erlang:now()],  "HBQTestLogging.log"}.

sort_test() ->
  Messages = [{11, "Bla"}, {12, "Blubb"}, {1, "Foo"}] ,
  [{1, "Foo"}, {11, "Bla"}, {12, "Blubb"}] == lists:keysort(1, Messages).