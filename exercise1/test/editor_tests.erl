-module(editor_tests).

-export([testClientExpectingMessage/0]).

-include_lib("eunit/include/eunit.hrl").

testClientExpectingMessage() ->
  receive
    {reply, _ReceivedMessage, _Terminated} ->
      ok
  end.

simple_test() ->

  {ok, Config} = file:consult("./config/server.cfg"),
  {ok, ServerName} = werkzeug:get_config_value(servername, Config),
  server:startMe(),
  editor:start("Juergen", [], 500, ServerName),
  timer:sleep(500),
  ClientPID = spawn(?MODULE, testClientExpectingMessage, []),
  %server:startMe("./test-config/server.cfg"),
  ServerName ! {ClientPID, getmessages},
  timer:sleep(500),
  ?assert(undefined =:= erlang:process_info(ClientPID)),
  ServerName ! terminate
.



