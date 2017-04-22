%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(starter_tests).
-include_lib("eunit/include/eunit.hrl").
-export([]).


simple_test() ->
  werkzeug:ensureNameserviceStarted(),
  koordinator:start("./test-config/koordinator.cfg"),
  starter:start(starterNo1).