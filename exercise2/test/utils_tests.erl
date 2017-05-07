%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(utils_tests).

-include_lib("eunit/include/eunit.hrl").

-export([name_service/0]).

name_service() ->
  receive
    {From, {lookup, Name}} ->
      From ! {pin, {Name, node()}};
    terminate -> ok
  end.

with_redefed_name_service(Name) ->
  PID = spawn(?MODULE, name_service, []),
  yes = global:register_name(Name, PID),
  PID.

simple_config() ->
  [{nameservicenode, node()}, {nameservicename, foobar}].

bind_name_service_test() ->
  Expected = with_redefed_name_service(foobar),
  ?assertEqual(Expected, utils:bind_nameservice(simple_config())),
  Expected ! terminate.


ceiling_test() ->
  ?assertEqual(utils:ceiling(5.3), 6),
  ?assertEqual(utils:ceiling(5.0), 5).