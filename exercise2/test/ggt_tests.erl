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
    {_From, {multicast, vote, _GgTName}} ->
      ok;
    terminate -> ok
  end.

coordinator(GgTName) ->
  receive
    {hello, GgTName} -> ok;
    {briefmi, {GgTName, _NewMi, _Timestamp}} -> ok
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
    workingtime => 1,
    termtime => 20,
    quota => 2,
    coordinator => Coordinator,
    nameservice => NameService,
    leftneighbor => LeftN,
    rightneigbor => RightN,
    mi => Mi,
    yesVotes => 0,
    terminateTimer => undefined,
    isTerminating => true}.

start_ggt_process_and_kill_test() ->
  % Setup
  NameServer = with_redefed_name_service(nameservice),
  Coordinator = with_redefed_coordinator('4321'),

  % Start a ggT process
  PID = ggt:start(2000, 20000, 6, '4321', Coordinator, NameServer),
  timer:sleep(100),
  ?assert(undefined =:= erlang:process_info(NameServer)),
  ?assert(undefined =:= erlang:process_info(Coordinator)),
  ?assert(undefined =/= erlang:process_info(PID)),

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

set_neighbours_test() ->
  Actual = ggt:update_neighbours('left', 'right', simple_state('undefined', 'nameservice', 'undefined', 'undefined', 0)),
  ?assertEqual('left', maps:get(leftneighbor, Actual)),
  ?assertEqual('right', maps:get(rightneigbor, Actual)).

set_pm_test() ->
  NewState = ggt:set_pm(123, simple_state('undefined', 'nameservice', 'undefined', 'undefined', 0)),
  ?assertNotEqual(0, maps:get(terminateTimer, NewState)),
  ?assertEqual(123, maps:get(mi, NewState)),
  ?assertEqual(false, maps:get(isTerminating, NewState)).

updating_new_mi_test() ->
  Coordinator = with_redefed_coordinator('testggT'),
  L = with_redefed_neighbour(3),
  R = with_redefed_neighbour(3),
  timer:sleep(100),
  ?assert(undefined =/= erlang:process_info(Coordinator)),
  ?assert(undefined =/= erlang:process_info(L)),
  ?assert(undefined =/= erlang:process_info(R)),

  NewState = ggt:maybe_update_mi(3, simple_state(Coordinator, undefined, L, R, 6)),
  timer:sleep(1500),
  ?assertEqual(3, maps:get(mi, NewState)),
  ?assertEqual(false, maps:get(isTerminating, NewState)),
  ?assert(undefined =:= erlang:process_info(Coordinator)),
  ?assert(undefined =:= erlang:process_info(L)),
  ?assert(undefined =:= erlang:process_info(R)).

updating_new_mi_do_nothing_test() ->
  NewState = ggt:maybe_update_mi(6, simple_state(undefined, undefined, undefined, undefined, 6)),
  ?assertEqual(6, maps:get(mi, NewState)),
  ?assertEqual(false, maps:get(isTerminating, NewState)).

term_request_test() ->
  NameService = with_redefed_name_service(nameservice),
  State = simple_state(undefined, NameService, undefined, undefined, 0),
  timer:sleep(100),
  ?assert(undefined =/= erlang:process_info(NameService)),

  ggt:term_request(State),
  timer:sleep(100),
  ?assert(undefined =/= erlang:process_info(NameService)),

  NewState = maps:update(isTerminating, false, State),
  ggt:term_request(NewState),
  timer:sleep(100),
  ?assert(undefined =:= erlang:process_info(NameService)).
