-module(end_to_end_tests).
-export([fake_server/1]).
-include_lib("eunit/include/eunit.hrl").

serverPID() -> wk.

fake_server(Message) ->
  receive
    {ClientPID, getmessages} -> ClientPID ! {reply, Message, true}
  end
.

getmessages_with_empty_dlq_and_dont_update_cmem_test() ->
  server:startMe("./test-config/server.cfg"),
  %client:startClient(serverPID(), "Test", [], 2000),
  editor:start_sending(1, "Test", [], 1, wk),
  serverPID() ! {self(), getmessages},
  receive
    {reply, Message, Terminated} -> ok
  end,
  timer:sleep(3000)
.

fake_server_test() ->
  MSG = [1, "Test", erlang:now()],
 ServerPID = spawn(?MODULE, fake_server, [MSG]),
  ServerPID ! {self(), getmessages},
  receive
    {reply, Message, Terminated} -> ok
  end,
  timer:sleep(3000)
.


%%receive
%%{reply, [NNr, Msg | _T], Terminated} ->
%%if NNr =:= -1 ->
%%ok
%%end
%%end,
%%wk ! terminate,
%%timer:sleep(3000)

