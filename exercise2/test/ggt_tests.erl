%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt_tests).

-include_lib("eunit/include/eunit.hrl").

-export([name_service/0, coordinator/1, mock_ggt/1]).

name_service() ->
  receive
    {From, {rebind, _GgTName, _Node}} ->
      From ! ok;
    terminate -> ok
  end.

coordinator(GgTName) ->
  receive
    {hello, GgTName} -> ok;
    {briefmi, {GGTName, _NewMi, _Timestamp}} -> ok
  end.

mock_ggt(Mi) ->
  receive
    {sendy, Mi} -> ok
  end.

with_redefed_coordinator(GgTName) ->
  PID = spawn(?MODULE, coordinator, [GgTName]),
  PID.

with_redefed_name_service(Name) ->
  PID = spawn(?MODULE, name_service, []),
  yes = global:register_name(Name, PID),
  PID.

with_redefed_neighbour(Mi) ->
  PID = spawn(?MODULE, mock_ggt, [Mi]),
  PID.

simple_state(Coordinator, NameService, LeftN, RightN, Mi) ->
  #{ggtname => 'testggT',
    workingtime => 3,
    termtime => 2,
    quota => 2,
    coordinator => Coordinator,
    nameservice => NameService,
    leftneighbor => LeftN,
    rightneigbor => RightN,
    mi => Mi,
    yesVotes => 0,
    lastMiUpdate => 0,
    isTerminating => false}.

start_ggt_process_and_kill_test() ->
  % Setup
  NameServer = with_redefed_name_service(nameservice),
  Coordinator = with_redefed_coordinator('4321'),

  % Start a ggT process
  PID = ggt:start(2000, 20000, 6, '4321', Coordinator, NameServer),
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(NameServer)),
  ?assert(undefined =:= erlang:process_info(Coordinator)),
  ?assertNotEqual(undefined, erlang:process_info(PID)),

  % Kill the ggT process
  PID ! kill,
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(PID)).


set_mi_and_tell_mi_test() ->
  NameService = with_redefed_name_service(nameservice),
  Coordinator = with_redefed_coordinator('4321'),
  PID = ggt:start(2000, 20000, 6, '4321', Coordinator, NameService),
  timer:sleep(100),

  PID ! {setpm, 3456},
  PID ! {self(), tellmi},
  receive
    {mi, Mi} -> ?assert(Mi =:= 3456)
  end,
  
  PID ! kill,
  timer:sleep(100),
  ?assert(undefined =:= erlang:process_info(PID)),
  ?assert(undefined =:= erlang:process_info(Coordinator)).


%%set_neigbours_test() ->
%%  % create a ring
%%  % dummy(mi = -1) <- left(mi = 9) -><- testProcess(mi = 6) -><- right(mi = 12) -> dummy(mi = -1)
%%  % if 3 is send to testProcess,
%%  % it will calculate the reminder 3 and send it to left & right. This should in a ggT of 3 in every process
%%
%%  NameServer = with_redefed_name_service(nameservice),
%%  Coordinator = with_redefed_coordinator('1234'),
%%  timer:sleep(100),
%%  GGTUnderTest = ggt:start(2000, 20000, 6, '4321', Coordinator, NameServer),
%%  Left = ggt:start(2000, 20000, 6, left, chef, nameservice),
%%  Right = ggt:start(2000, 20000, 6, right, chef, nameservice),
%%  Dummy = ggt:start(2000, 20000, 6, dummy, chef, nameservice),
%%  timer:sleep(100),
%%  GGTUnderTest ! {setneighbors, Left, Right},
%%  Left ! {setpm, 9},
%%  Right ! {setpm, 12},
%%  Left ! {setneighbors, Dummy, GGTUnderTest},
%%  Right ! {setneighbors, GGTUnderTest, Dummy},
%%  GGTUnderTest ! {setpm, 6},
%%  GGTUnderTest ! {sendy, 3},
%%  timer:sleep(100),
%%  Left ! {self(), tellmi},
%%  receive
%%    {mi, Mi} ->  io:format(lists:concat(["Mi received", Mi])), ?assert(Mi =:= 3)
%%  end,
%%  Right ! {self(), tellmi},
%%  receive
%%    {mi, Mi2} ->  io:format(lists:concat(["Mi received", Mi2])), ?assert(Mi =:= 3)
%%  end,
%%  GGTUnderTest ! kill,
%%  Left ! kill,
%%  Right ! kill,
%%  Dummy ! kill,
%%  NameServer ! terminate,
%%  timer:sleep(100),
%%  ?assertEqual(undefined, erlang:process_info(GGTUnderTest)).

set_neighbours_test() ->
  Actual = ggt:update_neighbours('left', 'right', simple_state('undefined', 'nameservice', 'undefined', 'undefined', 0)),
  ?assertEqual('left', maps:get(leftneighbor, Actual)),
  ?assertEqual('right', maps:get(rightneigbor, Actual)).

update_mi_state_test() ->
  NewState = ggt:update_mi_state(#{lastMiUpdate => 0, ggtname => 'testggT'}),
  ?assertNotEqual(0, maps:get(lastMiUpdate, NewState)).

set_pm_test() ->
  NewState = ggt:set_pm(123, simple_state('undefined', 'nameservice', 'undefined', 'undefined', 0)),
  ?assertNotEqual(0, maps:get(lastMiUpdate, NewState)),
  ?assertEqual(123, maps:get(mi, NewState)).

updating_new_mi_test() ->
  Coordinator = with_redefed_coordinator('ggt1'),
  L = with_redefed_neighbour(3),
  R = with_redefed_neighbour(3),
  timer:sleep(100),
  ?assert(undefined =/= erlang:process_info(Coordinator)),
  ?assert(undefined =/= erlang:process_info(L)),
  ?assert(undefined =/= erlang:process_info(R)),

  NewState = ggt:maybe_update_mi(3, simple_state(Coordinator, undefined, L, R, 6)),
  timer:sleep(100),
  ?assertEqual(3, maps:get(mi, NewState)),
  ?assert(undefined =:= erlang:process_info(Coordinator)),
  ?assert(undefined =:= erlang:process_info(L)),
  ?assert(undefined =:= erlang:process_info(R)).

updating_new_mi_do_nothing_test() ->
  NewState = ggt:maybe_update_mi(6, simple_state(undefined, undefined, undefined, undefined, 6)),
  ?assertEqual(6, maps:get(mi, NewState)).