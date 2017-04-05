-module(server_tests).

-include_lib("eunit/include/eunit.hrl").

-export([testClientGetMSGID1/0]).

testClientGetMSGID1() ->
  receive
    {nid, 1} -> ok
  end.

getmsgid_no_messages_yet_test() ->
  server:start(),
  ClientPID = spawn(?MODULE, testClientGetMSGID1, []),
  wk ! {ClientPID, getmsgid},
  timer:sleep(1000),
  undefined = erlang:process_info(ClientPID),
  wk ! terminate.

terminate_server_after_timeoutseconds_test() ->
  server:start(),
  timer:sleep(3000),
  undefined = erlang:process_info(wk).