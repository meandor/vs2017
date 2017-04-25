%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt_tests).

-include_lib("eunit/include/eunit.hrl").

-export([name_service/0, coordinator/1]).

name_service() ->
  receive
    {From, {rebind, _GgTName, _Node}} ->
      From ! ok;
    terminate -> ok
  end.

coordinator(GgTName) ->
  receive
    {hello, GgTName} -> ok
  end.

with_redefed_coordinator(GgTName) ->
  PID = spawn(?MODULE, coordinator, [GgTName]),
  PID.

with_redefed_name_service(Name) ->
  PID = spawn(?MODULE, name_service, []),
  yes = global:register_name(Name, PID),
  PID.

start_ggt_process_and_kill_test() ->
  % Setup
  NameServer = with_redefed_name_service(nameservice),
  Coordinator = with_redefed_coordinator('4321'),

  % Start a ggT process
  PID = ggt:start(2000, 20000, 6, '4321', Coordinator, NameServer),
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(NameServer)),
  ?assertEqual(undefined, erlang:process_info(Coordinator)),
  ?assertNotEqual(undefined, erlang:process_info(PID)),

  % Kill the ggT process
  PID ! kill,
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(PID)).

%%
%%set_mi_test() ->
%%  ensure_services(),
%%  start_test_ggt(),
%%  testProcessName() ! {setpm, 3456},
%%  testProcessName() ! {self(), tellmi},
%%  receive
%%    {mi, Mi} -> ?assert(Mi =:= 3456)
%%  end,
%%  kill_ggt().

%%set_neigbours_test() ->
%%  % create a ring
%%  % dummy(mi = -1) <- left(mi = 9) -><- testProcess(mi = 6) -><- right(mi = 12) -> dummy(mi = -1)
%%  % if 3 is send to testProcess,
%%  % it will calculate the reminder 3 and send it to left & right. This should in a ggT of 3 in every process
%%  ensure_services(),
%%  start_test_ggt(),
%%  ggt:start(2000, 20000, 6, left, chef, nameservice),
%%  ggt:start(2000, 20000, 6, right, chef, nameservice),
%%  ggt:start(2000, 20000, 6, dummy, chef, nameservice),
%%  timer:sleep(100),
%%  testProcessName() ! {setneighbors, left, right},
%%  left ! {setpm, 9},
%%  right ! {setpm, 12},
%%  left ! {setneighbors, dummy, testProcessName()},
%%  right ! {setneighbors, testProcessName(), dummy},
%%  testProcessName() ! {setpm, 6},
%%  testProcessName() ! {sendy, 3},
%%  timer:sleep(100),
%%  left ! {self(), tellmi},
%%  receive
%%    {mi, Mi} ->  io:format(lists:concat(["Mi received", Mi])), ?assert(Mi =:= 3)
%%  end,
%%  right ! {self(), tellmi},
%%  receive
%%    {mi, Mi} ->  io:format(lists:concat(["Mi received", Mi])), ?assert(Mi =:= 3)
%%  end,
%%  kill_ggt(),
%%  left ! kill,
%%  right ! kill,
%%  dummy ! kill
%%.
