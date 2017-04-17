%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  werkzeug:ensureNameserviceStarted(),
  koordinator:start(),
  GGTPid = ggt:start(2000,20000,6, c4321, chef, nameservice),
  timer:sleep(100),
  c4321 ! kill,
  timer:sleep(100),
  ?assert(whereis(c4321) =:= undefined).

