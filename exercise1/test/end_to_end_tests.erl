-module(end_to_end_tests).

-include_lib("eunit/include/eunit.hrl").

serverPID() -> wk.

fakeClient() ->
  receive
    {reply, Message, Terminated} -> ok
  end
.

getmessages_with_empty_dlq_and_dont_update_cmem_test() ->
  server:startMe("./test-config/server.cfg"),
  %client:startClient(serverPID(), "Test", [], 2000),
  editor:start_sending(1, "Test", [], 2000, wk),
  serverPID() ! {self(), getmessages},
  receive
    {reply, Message, Terminated} -> ok
  end

 .

%%receive
%%{reply, [NNr, Msg | _T], Terminated} ->
%%if NNr =:= -1 ->
%%ok
%%end
%%end,
%%wk ! terminate,
%%timer:sleep(3000)

