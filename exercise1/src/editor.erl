-module(editor).
-export([start_sending/5, receiveNNr/3, receiveLastNNr/1]).

calculateNewInterval(Interval) ->
  Increase = werkzeug:bool_rand(),
  Value = trunc(max(Interval * 0.5, 1000)),
  if
    Increase ->
      Interval + Value;
    true ->
      max(Interval - Value, 1000)
  end.

startLoop(ServerPID, Intervall) ->
  editor:start(clientLog(), Intervall, ServerPID, self()),
  receive
    {doneSending, ReaderNNrs} -> reader:start_reading(false, clientLog(), ReaderNNrs, ServerPID)
  end,
  startLoop(ServerPID, calculateNewInterval(Intervall)).


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

start_sending(0, Logfile, ReaderNNRs, SendWait, ServerPID) ->
  ReceiveLastServer = spawn(?MODULE, receiveLastNNr, [Logfile]),
  werkzeug:logging(Logfile, lists:concat(["EDITOR>>>", " Receive last\n"])),
  ServerPID ! {ReceiveLastServer, getmsgid};


start_sending(Counter, Logfile, ReaderNNrs, SendWait, ServerPID) ->
  ServerPID ! {self(), getmsgid},
  receive
    {nid, NextNNr} ->
      werkzeug:logging(Logfile, lists:concat(["EDITOR>>>", "Received NNr", NextNNr, "\n"])),
      NewReaderNNrs = ReaderNNrs ++ [NextNNr],
      %TODO Message format
      wk ! {dropmessage, [NextNNr, "Nachricht vom Editor", erlang:now()]},
      start_sending(Counter - 1, Logfile, NewReaderNNrs, SendWait, ServerPID)
  end,
  timer:sleep(SendWait),
  werkzeug:logging(Logfile, lists:concat(["EDITOR>>>", Counter, " ID reqeusted\n"])),

.