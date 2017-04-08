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
  server:startMe("./test-config/server.cfg"),
  editor:start("Juergen", [], 500, ServerName),
  timer:sleep(2001),
  ClientPID = spawn(?MODULE, testClientExpectingMessage, [1]),
  ServerName ! {ClientPID, getmessages},
  ?assert(undefined =:= erlang:process_info(ClientPID))

.



