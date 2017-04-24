-module(starter_tests).
-include_lib("eunit/include/eunit.hrl").
-export([]).

logging_test() ->
  Expected = list_to_atom(lists:concat(["ggt42@", atom_to_list(node()), ".log"])),
  ?assert(Expected =:= starter:log([{starterid, 42}], ["foobar"])).

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