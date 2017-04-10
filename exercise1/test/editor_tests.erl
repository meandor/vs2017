-module(editor_tests).

-include_lib("eunit/include/eunit.hrl").

testClientExpectingMessage() ->
  receive
    {reply, _ReceivedMessage, _Terminated} ->
      werkzeug:logging("Editor", "received requested message"),
      ok
  end.

message_test() ->
  ?assert(0 =/= string:rstr(editor:createMessage(132), "lab18-1-3: message_number_132. Sent time:")).
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



