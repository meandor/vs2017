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

with_redefed_nonterminating_name_service(Name) ->
  PID = spawn(?MODULE, nonterminating_name_service, []),
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

%%start_and_kill_test() ->
%%  NameService = with_redefed_name_service(foobar),
%%
%%  Testee = spawn(koordinator, init_coordinator, [simple_config()]),
%%
%%  timer:sleep(100),
%%  ?assertEqual(undefined, erlang:process_info(NameService)),
%%  ?assertNotEqual(undefined, erlang:process_info(Testee)),
%%
%%  Testee ! kill,
%%  timer:sleep(100),
%%  ?assertEqual(undefined, erlang:process_info(Testee)).

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

%%ringbuild_test() ->
%%  NameService = with_redefed_nonterminating_name_service(foobar2),
%%  Testee = spawn(koordinator, initial_state, [#{config => simple_config(), clients => []}]),
%%  Nodes = [one, two, three, four, five, six],
%%  start_nodes(Nodes, Testee, NameService),
%%  timer:sleep(1000),
%%  Testee ! step,
%%  timer:sleep(1000),
%%  send_node_command(Nodes, {setpm, 6}),
%%  one ! {sendy, 3},
%%  % Without this sleep not working
%%  timer:sleep(100),
%%
%%  NameService ! terminate,
%%  assert_three(Nodes),
%%
%%  send_node_command(Nodes, kill),
%%  Testee ! kill


start_nodes([], _Koordinator, _Nameservice) -> ok;
start_nodes([H | T], Koordinator, Nameservice) ->
  ggt:start(2000, 20000, 6, H, Koordinator, Nameservice),
  start_nodes(T, Koordinator, Nameservice).

send_node_command([], _Command) -> ok;
send_node_command([H | T], Command) ->
  H ! Command,
  send_node_command(T, Command).

twenty_percent_clients_test() ->
  Clients = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
  ?assertEqual(length(koordinator:twenty_percent_of(Clients)), 2).

update_minimum_test() ->
  ?assertEqual(koordinator:update_minimum(4, 5), 4),
  ?assertEqual(koordinator:update_minimum(1000, 3), 3)
.

handle_briefterm_test() ->
  Client =  spawn(?MODULE, fake_client, []),
  register(fake_ggt, Client),
  koordinator:handle_briefterm(50, 5, fake_ggt, erlang:now()),
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Client)).

kill_test() ->
  Testee = spawn(koordinator, initial_state, [#{config => simple_config(), clients => [fake_ggt1, fake_ggt2]}]),
  timer:sleep(100),
  Client1 =  spawn(?MODULE, fake_client, []),
  register(fake_ggt1, Client1),
  Client2 =  spawn(?MODULE, fake_client, []),
  register(fake_ggt2, Client2),
  timer:sleep(1000),
  Testee ! kill,
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Client1))
 %,?assertEqual(undefined, erlang:process_info(Client2))
.

assert_three([]) -> ok;
assert_three([H | T]) ->
  H ! {self(), tellmi},
  receive
    {mi, Mi} -> ?assert(Mi =:= 3)
  end,
  assert_three(T).
