-module(editor).
-export([start/4, receiveNNr/3, receiveLastNNr/1]).

receiveNNr(ServerPID, ReaderNNrs, Logging) ->
  receive
    {nid, NextNNr} ->
      werkzeug:logging(Logging, lists:concat(["EDITOR>>>", "Received NNr\n", NextNNr])),
      ReaderNNrs = lists:append(ReaderNNrs, [NextNNr]),
      %TODO Message format
      ServerPID ! {dropmessage, [NextNNr, "Nachricht vom Editor", erlang:now()]};
    {terminate, ClientPID} -> ClientPID ! {doneSending, ReaderNNrs}
  end,
  receiveNNr(ServerPID, ReaderNNrs, Logging)
.

receiveLastNNr(Logfile) ->
  receive
    {nid, NextNNr} -> werkzeug:logging(Logfile, lists:concat(["EDITOR>>> message number ", NextNNr, " forgotten\n"]))
  end.

start(Logfile, SendWait, ServerPID, ClientPID) ->
  werkzeug:logging(Logfile, lists:concat(["EDITOR>>>", " Receiving requested NNrs\n"])),
  ReceiveServer = spawn(?MODULE, receiveNNr, [ServerPID, [], Logfile]),
  start_sending(0, Logfile, SendWait, ServerPID, ClientPID, ReceiveServer).

start_sending(5, Logfile, _SendWait, ServerPID, ClientPID, ReceiveServer) ->
  ReceiveLastServer = spawn(?MODULE, receiveLastNNr, [Logfile]),
  ServerPID ! {ReceiveLastServer, getmsgid},
  ReceiveServer ! {terminate, ClientPID};

start_sending(Counter, Logfile, SendWait, ServerPID, ClientPID, ReceiveServer) ->
  ServerPID ! {ReceiveServer, getmsgid},
  timer:sleep(SendWait),
  werkzeug:logging(Logfile, lists:concat(["EDITOR>>>", Counter, " ID reqeusted\n"])),
  start_sending(Counter + 1, Logfile, SendWait, ServerPID, ClientPID, ReceiveServer)
.



