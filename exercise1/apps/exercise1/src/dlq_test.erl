%%%-------------------------------------------------------------------
%%% @author fawin
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Mar 2017 14:18
%%%-------------------------------------------------------------------
-module(dlq_test).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

init_test() -> {[], 2, "Test"} = dlq:initDLQ(2, "Test").