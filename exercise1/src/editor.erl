-module(editor).
-export([start/4, receiveNNr/2, receiveLastNNr/1]).

receiveNNr(ServerPID, ReaderNNrs) ->
  receive
    {nid, NextNNr} ->
      werkzeug:logging("Juergen", lists:concat(["EDITOR>>>", "Reveived NNr\n", NextNNr])),
     % ReaderNNrs = lists:append(ReaderNNrs, [NextNNr]),
      %TODO
      ServerPID ! {dropmessage, [NextNNr, "Nachricht vom Editor", erlang:now()]}
  end,
  receiveNNr(ServerPID, ReaderNNrs)
.

receiveLastNNr(Logfile) ->
  receive
    {nid, NextNNr} -> werkzeug:logging(Logfile, lists:concat(["EDITOR>>> message number ", NextNNr, " forgotten\n"]))
  end.

start(Logfile, ReaderNNrs, SendWait, ServerPID) ->
  werkzeug:logging(Logfile, lists:concat(["EDITOR>>>", " Receiving requested NNrs\n"])),
  ReceiveServer = spawn(?MODULE, receiveNNr, [ServerPID, ReaderNNrs]),
  start_sending(0, Logfile, ReaderNNrs, SendWait, ServerPID, ReceiveServer).
start_sending(5, Logfile, ReaderNNrs, _SendWait, ServerPID, _ReceiveServer) ->
  ReceiveLastServer = spawn(?MODULE, receiveLastNNr, [Logfile]),
  ServerPID ! {ReceiveLastServer, getmsgid},
  ReaderNNrs;

start_sending(Counter, Logfile, ReaderNNrs, SendWait, ServerPID, ReceiveServer) ->
  ServerPID ! {ReceiveServer, getmsgid},
  timer:sleep(SendWait),
  werkzeug:logging(Logfile, lists:concat(["EDITOR>>>", Counter, " ID reqeusted\n"])),
  start_sending(Counter + 1, Logfile, ReaderNNrs, SendWait, ServerPID, ReceiveServer)
.



