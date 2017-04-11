-module(end_to_end_tests).
-export([fake_server/1]).
-include_lib("eunit/include/eunit.hrl").

serverPID() -> wk.

fake_server([NNr, Message | Rest]) ->
  receive
    {ClientPID, getmessages} -> ClientPID ! {reply, [NNr, Message] ++ Rest, true}
  end,
  fake_server([NNr - 1, Message] ++ Rest)
;

fake_server([1, Message | Rest]) ->
  receive
    {ClientPID, getmessages} -> ClientPID ! {reply, [1, Message] ++ Rest, false}
  end
.


waitingTime() -> 1000.

%
editor_start_sending_test() ->
  server:startMe("./test-config/server.cfg"),
  %client:startClient(serverPID(), "Test", [], 2000),
  editor:start_sending(1, "Test", [], 1, wk),
  serverPID() ! {self(), getmessages},
  receive
    {reply, [NNr, _Msg | _Timestamps ], Terminated} ->
      timer:sleep(waitingTime()),
      ?assert((NNr =:= 1) and (Terminated))
  end
.

%This tests if 4 messages are read from the server, after they are returned by a mock server
fake_server_test() ->
  MSG = [4, "Test", erlang:now(), erlang:now(),erlang:now(), erlang:now()],
  ServerPID = spawn(?MODULE, fake_server, [MSG]),
  {ReaderNNrs, _Logfile} = reader:start_reading(false, "Test", [], ServerPID),
  %io:format("~w~n", [ReaderNNrs]),
  ?assert(ReaderNNrs =:= [4])
.

%% DLQ Size ist als 6 definiert. Wenn wir die erste nachricht vergessen und danach 5 weitere schreiben, sollten wir nachricht 1 bis 6
%% bei get Messages erhalten
filled_hbq_test() ->
  server:startMe("./test-config/server.cfg"),
  % forget id 1
  editor:start_sending(0, "Test", [], 1, wk),
  editor:start_sending(5, "Test", [], 1, wk),
  io:format(lists:concat(["receive_n ", 1,5, "\n"])),
  receive_n(1, 6)
.

receive_n(Max, Max) -> ok;
receive_n(N, Max) ->
  io:format(lists:concat(["receive_n ", N, Max, "\n"])),
  serverPID() ! {self(), getmessages},
  receive
    {reply, [NNr, _Msg | _Timestamps ], false} ->  io:format(lists:concat(["Number received: ", NNr, "\n"])), ?assert((NNr == N))
  end,
  receive_n(N + 1, Max).




%%receive
%%{reply, [NNr, Msg | _T], Terminated} ->
%%if NNr =:= -1 ->
%%ok
%%end
%%end,
%%wk ! terminate,
%%timer:sleep(3000)

