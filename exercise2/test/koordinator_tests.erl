%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for the coordination module
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  ?assert(true).

ringbuild_test() ->
  werkzeug:ensureNameserviceStarted(),
  koordinator:start("./test-config/koordinator.cfg"),
  Nodes = [one, two, three, four, five, six],
  start_nodes(Nodes),
  timer:sleep(1000),
  chef ! step,
  timer:sleep(1000),
  send_node_command(Nodes, {setpm, 6}),
  three ! {sendy, 3},
  % Without this sleep not working
  timer:sleep(100),
  assert_three(Nodes),

  send_node_command(Nodes, kill),
  chef ! kill
.

kill_test() ->
  werkzeug:ensureNameserviceStarted(),
  koordinator:start("./test-config/koordinator.cfg"),
  chef ! kill,
  timer:sleep(100),
  ?assert(whereis(chef)  =:= undefined)
.

steering_interval_test() ->
  koordinator:start("./test-config/koordinator.cfg"),
  chef ! {self(), getsteeringval},
  receive
    {steeringval,WorkingTime,TerminationTime,Quota,GGTProcessNumber}-> ?assert(true)
  end
.


start_nodes([]) -> ok;
start_nodes([H | T]) ->
  ggt:start(2000, 20000, 6, H, chef, nameservice),
  start_nodes(T).

send_node_command([], _Command) -> ok;
send_node_command([H | T], Command) ->
  H ! Command,
  send_node_command(T, Command).



assert_three([]) -> ok;
assert_three([H | T]) ->
  H ! {self(), tellmi},
  receive
    {mi, Mi} -> ?assert(Mi =:= 3)
  end,
  assert_three(T).
