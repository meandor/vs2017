%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for the coordination module
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator_tests).

-include_lib("eunit/include/eunit.hrl").

-export([name_service/1, mock_starter/4, nonterminating_name_service/0, ggT_process/0]).

nonterminating_name_service() ->
  receive
    {From, {rebind, _Coordfoobar, Node}} ->
      Node = node(),
      From ! ok,
      name_service(#{});
    terminate -> ok
  end.

name_service(PIDMap) ->
  receive
    {From, {rebind, coordfoobar, Node}} ->
      Node = node(),
      From ! ok;
    {PID, {lookup, GgTName}} ->
      NewPIDMap = maps:remove(GgTName, PIDMap),
      NewPIDMapSize = maps:size(NewPIDMap),
      PID ! {pin, maps:get(GgTName, PIDMap)},
      if
        NewPIDMapSize > 0 -> name_service(NewPIDMap);
        true -> ok
      end;
    terminate -> ok
  end.

ggT_process() ->
  receive
    {sendy, _Mi} -> ok;
    {setpm, _Mi} -> ok;
    {setneighbors, _LeftNeighbour, _RightNeighbour} -> ok;
    kill -> ok
  end.

mock_starter(WorkingTime, TermTime, Quota, Processes) ->
  receive
    {steeringval, WorkingTime, TermTime, Quota, Processes} -> ok
  end.

with_redefed_name_service(Name, PIDMap) ->
  PID = spawn(?MODULE, name_service, [PIDMap]),
  yes = global:register_name(Name, PID),
  PID.

with_redefed_ggt(GgTName) ->
  PID = spawn(?MODULE, ggT_process, []),
  erlang:register(GgTName, PID),
  PID.

simple_config() ->
  [{nameservicenode, node()},
    {nameservicename, foobar},
    {koordinatorname, coordfoobar},
    {arbeitszeit, 2},
    {termzeit, 42},
    {ggtprozessnummer, 3},
    {quote, 80},
    {korrigieren, 1}].

simple_state(Clients) ->
  #{config => simple_config(), clients => Clients, clientsToPID => #{}, smallestGgT => 5}.

start_and_kill_test() ->
  NameService = with_redefed_name_service(foobar, #{}),

  Testee = spawn(koordinator, init_coordinator, [simple_config()]),

  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(NameService)),
  ?assertNotEqual(undefined, erlang:process_info(Testee)),

  Testee ! kill,
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Testee)).

get_steering_interval_and_kill_test() ->
  Testee = spawn(koordinator, initial_state, [#{config => simple_config(), clients => []}]),
  Starter = spawn(?MODULE, mock_starter, [2, 42, 2, 3]),
  timer:sleep(100),

  ?assertNotEqual(undefined, erlang:process_info(Testee)),
  ?assertNotEqual(undefined, erlang:process_info(Starter)),
  Testee ! {Starter, getsteeringval},
  timer:sleep(100),

  % Starter got correct values
  ?assertEqual(undefined, erlang:process_info(Starter)),
  Testee ! kill,
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Testee)).

send_hello_test() ->
  Testee = spawn(koordinator, initial_state, [#{config => simple_config(), clients => []}]),
  timer:sleep(100),

  ?assertNotEqual(undefined, erlang:process_info(Testee)),
  Testee ! {hello, foobar1},
  timer:sleep(100),
  ?assertNotEqual(undefined, erlang:process_info(Testee)),

  Testee ! kill,
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Testee)).

ring_build_with_3_ggTs_test() ->
  State = simple_state([foobar1, foobar2, foobar3]),
  GgT1 = with_redefed_ggt(foobar1),
  GgT2 = with_redefed_ggt(foobar2),
  GgT3 = with_redefed_ggt(foobar3),
  NameService = with_redefed_name_service(foobar, #{foobar1 => GgT1, foobar2=>GgT2, foobar3=>GgT3}),

  PID = spawn(koordinator, transition_to_calculation_state, [State]),
  timer:sleep(100),
  PID ! kill,
  timer:sleep(100),

  ?assertEqual(undefined, erlang:process_info(PID)),
  ?assertEqual(undefined, erlang:process_info(GgT1)),
  ?assertEqual(undefined, erlang:process_info(GgT2)),
  ?assertEqual(undefined, erlang:process_info(GgT3)),
  ?assertEqual(undefined, erlang:process_info(NameService)).

ring_build_with_2_ggTs_test() ->
  State = simple_state([foobar1, foobar2]),
  GgT1 = with_redefed_ggt(foobar1),
  GgT2 = with_redefed_ggt(foobar2),
  NameService = with_redefed_name_service(foobar, #{foobar1 => GgT1, foobar2=>GgT2}),

  PID = spawn(koordinator, transition_to_calculation_state, [State]),
  timer:sleep(100),
  PID ! kill,
  timer:sleep(100),

  ?assertEqual(undefined, erlang:process_info(PID)),
  ?assertEqual(undefined, erlang:process_info(GgT1)),
  ?assertEqual(undefined, erlang:process_info(GgT2)),
  ?assertEqual(undefined, erlang:process_info(NameService)).

ring_build_with_4_ggTs_test() ->
  State = simple_state([foobar1, foobar2, foobar3, foobar4]),
  GgT1 = with_redefed_ggt(foobar1),
  GgT2 = with_redefed_ggt(foobar2),
  GgT3 = with_redefed_ggt(foobar3),
  GgT4 = with_redefed_ggt(foobar4),
  NameService = with_redefed_name_service(foobar, #{foobar1 => GgT1, foobar2=>GgT2, foobar3 => GgT3, foobar4=>GgT4}),

  PID = spawn(koordinator, transition_to_calculation_state, [State]),
  timer:sleep(100),
  PID ! kill,
  timer:sleep(100),

  ?assertEqual(undefined, erlang:process_info(PID)),
  ?assertEqual(undefined, erlang:process_info(GgT1)),
  ?assertEqual(undefined, erlang:process_info(GgT2)),
  ?assertEqual(undefined, erlang:process_info(GgT3)),
  ?assertEqual(undefined, erlang:process_info(GgT4)),
  ?assertEqual(undefined, erlang:process_info(NameService)).

twenty_percent_clients_test() ->
  Clients = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
  ?assertEqual(2, length(koordinator:twenty_percent_of(Clients))).

%%update_minimum_test() ->
%%  ?assertEqual(koordinator:update_minimum(4, 5), 4),
%%  ?assertEqual(koordinator:update_minimum(1000, 3), 3).

handle_briefterm_test() ->
  Client = spawn(?MODULE, ggT_process, []),
  State = maps:update(clientsToPID, #{fake_ggt => Client}, simple_state([fake_ggt])),
  register(fake_ggt, Client),
  koordinator:handle_briefterm(State, 50, fake_ggt, erlang:now()),
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Client)).

kill_test() ->
  Testee = spawn(koordinator, initial_state, [#{config => simple_config(), clients => [fake_ggt1, fake_ggt2]}]),
  timer:sleep(100),
  Client1 = spawn(?MODULE, ggT_process, []),
  register(fake_ggt1, Client1),
  Client2 = spawn(?MODULE, ggT_process, []),
  register(fake_ggt2, Client2),
  timer:sleep(1000),
  Testee ! kill,
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Client1)),
  ?assertEqual(undefined, erlang:process_info(Client2)).

set_initial_mis_test() ->
  Mis = werkzeug:bestimme_mis(4, 3),
  Client1 = spawn(?MODULE, ggT_process, []),
  register(fake_ggt1, Client1),
  Client2 = spawn(?MODULE, ggT_process, []),
  register(fake_ggt2, Client2),
  Client3 = spawn(?MODULE, ggT_process, []),
  register(fake_ggt3, Client3),
  Clients = [fake_ggt1, fake_ggt2, fake_ggt3],
  koordinator:set_initial_mis(Mis, Clients, #{fake_ggt1 => Client1, fake_ggt2 => Client2, fake_ggt3 => Client3}),
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Client1)),
  ?assertEqual(undefined, erlang:process_info(Client2)),
  ?assertEqual(undefined, erlang:process_info(Client3)).
