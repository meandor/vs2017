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
  ggt:start(2000, 20000, 6, one, chef, nameservice),
  ggt:start(2000, 20000, 6, two, chef, nameservice),
  ggt:start(2000, 20000, 6, three, chef, nameservice),
  ggt:start(2000, 20000, 6, four, chef, nameservice),
  ggt:start(2000, 20000, 6, five, chef, nameservice),
  ggt:start(2000, 20000, 6, six, chef, nameservice),
  timer:sleep(1000),
  chef ! step,
  timer:sleep(1000),

  one ! {setpm, 6},
  two ! {setpm, 6},
  three ! {setpm, 6},
  four ! {setpm, 6},
  five ! {setpm, 6},
  six ! {setpm, 6},
  three ! {sendy, 3},
  four ! {self(), tellmi},

  one ! kill,
  two ! kill,
  three ! kill,
  four ! kill,
  five ! kill,
  six ! kill,
  chef! kill,
receive
{mi, Mi} -> ?assert(Mi =:= 6)
end.

  %,

