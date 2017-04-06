-module(end_to_end_tests).

-include_lib("eunit/include/eunit.hrl").

serverPID() -> wk.

getmessages_with_empty_dlq_and_dont_update_cmem_test() ->
  ClientPID = dlq_tests:testClientExpectingMessage([-1, "No new messages", -1, -1, -1], true),
  server:startMe("./test-config/server.cfg"),
  serverPID() ! {ClientPID, getmessages},
  timer:sleep(100),
  ?assert(undefined =:= erlang:process_info(ClientPID)).

% Add test for sending message and get request for test client
