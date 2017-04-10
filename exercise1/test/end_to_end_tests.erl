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

send_receive_test() ->
  client:start().

%%
%%simple_test() ->
%%
%%  {ok, Config} = file:consult("./config/server.cfg"),
%%  {ok, ServerName} = werkzeug:get_config_value(servername, Config),
%%  ServerPID = server:startMe(),
%%  timer:sleep(1000),
%%  editor:start("Editor", 100, ServerPID, self()),
%%  receive
%%    {doneSending, ReaderNNrs} -> io:format("reveiced done"), ok %?assert(ReaderNNrs =:= [1, 2, 3, 4, 5])
%%  end,
%%  ClientPID = spawn(?MODULE, testClientExpectingMessage, []),
%%  ?assert(undefined =/= ClientPID),
%%  ServerPID ! {ClientPID, getmessages},
%%  timer:sleep(2000),
%%  ?assert(undefined =:= erlang:process_info(ClientPID)),
%%  ServerPID ! terminate
%%.
