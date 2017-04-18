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
  end,
  kill_ggt().

set_neigbours_test() ->
  % create a ring
  % dummy(mi = -1) <- left(mi = 9) -><- testProcess(mi = 6) -><- right(mi = 12) -> dummy(mi = -1)
  % if 3 is send to testProcess,
  % it will calculate the reminder 3 and send it to left & right. This should in a ggT of 3 in every process
  ensure_services(),
  start_test_ggt(),
  ggt:start(2000, 20000, 6, left, chef, nameservice),
  ggt:start(2000, 20000, 6, right, chef, nameservice),
  ggt:start(2000, 20000, 6, dummy, chef, nameservice),
  timer:sleep(100),
  testProcessName() ! {setneighbors, left, right},
  left ! {setpm, 9},
  right ! {setpm, 12},
  left ! {setneighbors, dummy, testProcessName()},
  right ! {setneighbors, testProcessName(), dummy},
  testProcessName() ! {setpm, 6},
  testProcessName() ! {sendy, 3},
  timer:sleep(100),
  left ! {self(), tellmi},
  receive
    {mi, Mi} ->  io:format(lists:concat(["Mi received", Mi])), ?assert(Mi =:= 3)
  end,
  right ! {self(), tellmi},
  receive
    {mi, Mi} ->  io:format(lists:concat(["Mi received", Mi])), ?assert(Mi =:= 3)
  end,
  kill_ggt(),
  left ! kill,
  right ! kill,
  dummy ! kill
.





