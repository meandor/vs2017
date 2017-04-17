%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt_tests).

-include_lib("eunit/include/eunit.hrl").

ensure_services() ->
  werkzeug:ensureNameserviceStarted(),
  koordinator:start()
% , timer:sleep(100)
.

testProcessName() ->
  list_to_atom("c1234").

start_test_ggt() ->
  ggt:start(2000, 20000, 6, testProcessName(), chef, nameservice),
  timer:sleep(100).

kill_ggt() ->
  testProcessName() ! kill,
  timer:sleep(100),
  ?assert(whereis(testProcessName()) =:= undefined).

start_kill_test() ->
  ensure_services(),
  start_test_ggt(),
  kill_ggt().

set_mi_test() ->
  ensure_services(),
  start_test_ggt(),
  testProcessName() ! {setpm, 3456},
  testProcessName() ! {self(), tellmi},
  receive
    {mi, Mi} -> ?assert(Mi =:= 3456)
  end.

set_neigbours_test() ->
  ensure_services(),
  start_test_ggt(),
  ggt:start(2000, 20000, 6, left, chef, nameservice),
  ggt:start(2000, 20000, 6, right, chef, nameservice),
  testProcessName() ! {setneighbors, left, right}.
 % TODO




