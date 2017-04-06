-module(server_tests).

-include_lib("eunit/include/eunit.hrl").

-export([testClientGetMSGID/1]).

serverPID() -> wk.

testClientGetMSGID(NNr) ->
  receive
    {nid, NNr} -> ok
  end.

terminate_server_after_timeoutseconds_test() ->
  server:startMe("./test-config/server.cfg"),
  timer:sleep(2000),
  ?assert(undefined =:= whereis(serverPID())).

getmsgid_consecutive_ids_test() ->
  ClientPID = spawn(?MODULE, testClientGetMSGID, [1]),
  server:startMe("./test-config/server.cfg"),
  serverPID() ! {ClientPID, getmsgid},
  timer:sleep(100),
  ?assert(undefined =:= erlang:process_info(ClientPID)),
  ClientPID2 = spawn(?MODULE, testClientGetMSGID, [2]),
  serverPID() ! {ClientPID2, getmsgid},
  timer:sleep(100),
  ?assert(undefined =:= erlang:process_info(ClientPID)),
  timer:sleep(2000),
  ?assert(undefined =:= whereis(serverPID())).

getmsgid_no_messages_yet_and_dont_terminate_with_client_requests_test() ->
  ClientPID = spawn(?MODULE, testClientGetMSGID, [1]),
  server:startMe("./test-config/server.cfg"),
  serverPID() ! {ClientPID, getmsgid},
  timer:sleep(1000),
  ?assert(undefined =:= erlang:process_info(ClientPID)),
  ?assert(undefined =/= whereis(serverPID())),
  timer:sleep(2000),
  ?assert(undefined =:= whereis(serverPID())).
