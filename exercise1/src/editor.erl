-module(editor).
-export([start/4, receiveNNr/3, receiveLastNNr/1]).

receiveNNr(ServerPID, ReaderNNrs, Logging) ->
  receive
    {nid, NextNNr} ->
      werkzeug:logging(Logging, lists:concat(["EDITOR>>>", "Received NNr", NextNNr, "\n"])),
      NewReaderNNrs = ReaderNNrs ++ [NextNNr],
      %TODO Message format
      wk ! {dropmessage, [NextNNr, "Nachricht vom Editor", erlang:now()]},
      receiveNNr(ServerPID, NewReaderNNrs, Logging);
    {terminate, ClientPID} -> werkzeug:logging(Logging, lists:concat(["EDITOR>>>", "Terminating, sending done", "\n"])),
      ClientPID ! {doneSending, ReaderNNrs}, exit("Terminated"), ok
  end.

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
  werkzeug:logging(Logfile, lists:concat(["EDITOR>>>", " Receive last\n"])),
  ServerPID ! {ReceiveLastServer, getmsgid},
  ReceiveServer ! {terminate, ClientPID};

start_sending(Counter, Logfile, SendWait, ServerPID, ClientPID, ReceiveServer) ->
  ServerPID ! {ReceiveServer, getmsgid},
  timer:sleep(SendWait),
  werkzeug:logging(Logfile, lists:concat(["EDITOR>>>", Counter, " ID reqeusted\n"])),
  start_sending(Counter + 1, Logfile, SendWait, ServerPID, ClientPID, ReceiveServer)
.