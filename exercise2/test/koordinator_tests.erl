%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for the coordination module
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator_tests).

-include_lib("eunit/include/eunit.hrl").

-export([name_service/0, starter/4, nonterminating_name_service/0, fake_client/0]).

nonterminating_name_service() ->
  receive
    {From, {rebind, _Coordfoobar, Node}} ->
      Node = node(),
      From ! ok,
      name_service();
    terminate -> ok
  end.

name_service() ->
  receive
    {From, {rebind, coordfoobar, Node}} ->
      Node = node(),
      From ! ok;
    terminate -> ok
  end
.

fake_client() ->
  receive
    {sendy, Mi} -> ok;
    {setpm, Mi2} -> ok;
    kill -> ok
  end.

starter(WorkingTime, TermTime, Quota, Processes) ->
  receive
    {steeringval, WorkingTime, TermTime, Quota, Processes} -> ok
  end.

with_redefed_name_service(Name) ->
  PID = spawn(?MODULE, name_service, []),
  yes = global:register_name(Name, PID),
  PID.

simple_config() ->
  [{nameservicenode, node()},
    {nameservicename, foobar},
    {koordinatorname, coordfoobar},
    {arbeitszeit, 2},
    {termzeit, 42},
    {ggtprozessnummer, 2},
    {quote, 80},
    {korrigieren, 1}].

start_and_kill_test() ->
  NameService = with_redefed_name_service(foobar),

  Testee = spawn(koordinator, init_coordinator, [simple_config()]),

  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(NameService)),
  ?assertNotEqual(undefined, erlang:process_info(Testee)),

  Testee ! kill,
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Testee)).

get_steering_interval_and_kill_test() ->
  Testee = spawn(koordinator, initial_state, [#{config => simple_config(), clients => []}]),
  Starter = spawn(?MODULE, starter, [2, 42, 2, 2]),
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

ringbuild_with_3_ggTs_test() ->
  ok.


twenty_percent_clients_test() ->
  Clients = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
  ?assertEqual(length(koordinator:twenty_percent_of(Clients)), 2).

update_minimum_test() ->
  ?assertEqual(koordinator:update_minimum(4, 5), 4),
  ?assertEqual(koordinator:update_minimum(1000, 3), 3).

handle_briefterm_test() ->
  Client = spawn(?MODULE, fake_client, []),
  register(fake_ggt, Client),
  koordinator:handle_briefterm(50, 5, fake_ggt, erlang:now()),
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Client)).

kill_test() ->
  Testee = spawn(koordinator, initial_state, [#{config => simple_config(), clients => [fake_ggt1, fake_ggt2]}]),
  timer:sleep(100),
  Client1 = spawn(?MODULE, fake_client, []),
  register(fake_ggt1, Client1),
  Client2 = spawn(?MODULE, fake_client, []),
  register(fake_ggt2, Client2),
  timer:sleep(1000),
  Testee ! kill,
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Client1)),
  ?assertEqual(undefined, erlang:process_info(Client2)).

set_initial_mis_test() ->
  Mis = werkzeug:bestimme_mis(4, 3),
  Client1 = spawn(?MODULE, fake_client, []),
  register(fake_ggt1, Client1),
  Client2 = spawn(?MODULE, fake_client, []),
  register(fake_ggt2, Client2),
  Client3 = spawn(?MODULE, fake_client, []),
  register(fake_ggt3, Client3),
  Clients = [fake_ggt1, fake_ggt2, fake_ggt3],
  koordinator:set_initial_mis(Mis, Clients),
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Client1)),
  ?assertEqual(undefined, erlang:process_info(Client2)),
  ?assertEqual(undefined, erlang:process_info(Client3)).
