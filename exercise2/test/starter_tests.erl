-module(starter_tests).
-include_lib("eunit/include/eunit.hrl").
-export([name_service/0, coordinator/0]).

name_service() ->
  receive
    {From, {lookup, Name}} ->
      From ! {pin, {Name, node()}};
    terminate -> ok
  end.

coordinator() ->
  receive
    {From, getsteeringval} ->
      From ! {steeringval, 1, 42, 1337, 1}, % {steeringval, ArbeitsZeit, TermZeit, Quota, GGTProzessnummer}
      ok
  end.

with_redefed_name_service(Name) ->
  PID = spawn(?MODULE, name_service, []),
  yes = global:register_name(Name, PID),
  PID.

with_redefed_coordinator() ->
  PID = spawn(?MODULE, coordinator, []),
  PID.

simple_config() ->
  [{starterid, 42}, {nameservicenode, node()}, {nameservicename, foobar}, {koordinatorname, coordfoobar}].

logging_test() ->
  Expected = list_to_atom(lists:concat(["ggt42@", atom_to_list(node()), ".log"])),
  ?assert(Expected =:= starter:log([{starterid, 42}], ["foobar"])).

discover_coordinator_test() ->
  NameService = with_redefed_name_service(foobar),
  StarterPID = spawn(starter, discover_coordinator, [NameService, simple_config()]),
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(StarterPID)),
  ?assertEqual(undefined, erlang:process_info(NameService)).

get_steering_values_test() ->
  Coordinator = with_redefed_coordinator(),
  StarterPID = spawn(starter, get_steering_values, [Coordinator, simple_config()]),
  timer:sleep(100),
  ?assertEqual(undefined, erlang:process_info(Coordinator)),
  ?assertEqual(undefined, erlang:process_info(StarterPID)).

ggt_id_test_() -> [
  ?_assertEqual('4321', starter:ggT_id(4, 3, 2, 1)),
  ?_assertEqual('1337', starter:ggT_id(1, 3, 3, 7))
].

param_map() ->
  #{worktime => 1337,
    termtime => 42,
    starterid => 147,
    groupnumber => 1,
    teamnumber => 3,
    nameservice => {foo, bar},
    coordinator => something,
    quota => 7}.

start_ggT_processes_test() ->
  starter:start_ggT_processes(4, param_map(), fun(_1, _2, _3, Counter, _5, _6) -> Counter end, []),
  Actual = starter:start_ggT_processes(4, param_map(), fun(1337, 42, 7, Counter, something, {foo, bar}) ->
    Counter end, []),

  ?assertEqual(['134147', '133147', '132147', '131147'], Actual).
