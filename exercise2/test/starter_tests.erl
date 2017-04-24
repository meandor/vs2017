-module(starter_tests).
-include_lib("eunit/include/eunit.hrl").
-export([name_service/0]).

name_service() ->
  receive
    terminate -> ok
  end.

withRedefedNameService(Name) ->
  PID = spawn(?MODULE, name_service, []),
  yes = global:register_name(Name, PID),
  PID.

logging_test() ->
  Expected = list_to_atom(lists:concat(["ggt42@", atom_to_list(node()), ".log"])),
  ?assert(Expected =:= starter:log([{starterid, 42}], ["foobar"])).

bind_name_service_test() ->
  Expected = withRedefedNameService(foobar),
  ?assertEqual(Expected, starter:bind_nameservice([{starterid, 42}, {nameservicenode, node()}, {nameservicename, foobar}])),
  Expected ! terminate.

%%simple_test() ->
%%  werkzeug:ensureNameserviceStarted(),
%%  koordinator:start("./test-config/koordinator.cfg"),
%%  StarterNumber = list_to_atom(integer_to_list(1)),
%%  starter:start(StarterNumber),
%%  ClientName = list_to_atom(lists:append(integer_to_list(4881), "1")),
%%  ClientName ! {setpm, 3456},
%%  ClientName ! {self(), tellmi},
%%  receive
%%    {mi, Mi} -> ?assert(Mi =:= 3456)
%%  end
%%.