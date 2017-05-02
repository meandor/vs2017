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


set_mi_test() ->
  NameServer = with_redefed_name_service(nameservice),
  Coordinator = with_redefed_coordinator('1234'),
  PID = ggt:start(2000, 20000, 6, '4321', Coordinator, NameServer),
  timer:sleep(100),
  PID ! {setpm, 3456},
  PID ! {self(), tellmi},
  receive
    {mi, Mi} -> ?assert(Mi =:= 3456)
  end,
  PID ! kill,
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(PID)).


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
