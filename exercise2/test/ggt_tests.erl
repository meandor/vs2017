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
  ggt:start(2000,20000,6, c4321, chef, "./test-config/ggt.cfg"),
  ?assert(true).

