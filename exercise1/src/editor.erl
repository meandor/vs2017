%%%-------------------------------------------------------------------
%%% @doc
%%% Detailed Documentation: See section 3.3 of docs/aufgabe1_dokumentation.pdf
%%%
%%% This module sends 5 messages to the server, then requests a NNr without sending a message.
%%% Gets called from client in a loop.
%%% @end
%%%-------------------------------------------------------------------
-module(editor).
-export([start_sending/5, createMessage/1, calculateNewInterval/1]).

minInterval() -> 2000.

calculateNewInterval(Interval) ->
  Increase = werkzeug:bool_rand(),
  Value = trunc(max(Interval * 0.5, minInterval())),
  if
    Increase ->
      Interval + Value;
    true ->
      max(Interval - Value, minInterval())
  end.

% 0-client@lab18-<0.1313.0>-C-1-03: 22te_Nachricht. Sendezeit: 16.05 18:01:30,769|
createMessage(NNr) ->
  {ok, Config} = file:consult("./config/client.cfg"),
  {ok, Rechnername} = werkzeug:get_config_value(rechnername, Config),
  {ok, Prgroup} = werkzeug:get_config_value(praktikumsgruppe, Config),
  {ok, Teamnr} = werkzeug:get_config_value(teamnummer, Config),
  lists:concat([Rechnername, "-", Prgroup, "-", Teamnr, ": message_number_", integer_to_list(NNr), ". Sent time: ", werkzeug:timeMilliSecond()]).

% client_<Nummer><Node>.log
getLogFile([], NextNNr) ->
  Logfile = erlang:list_to_atom(lists:concat(["client_", NextNNr, node(), ".log"])),
  werkzeug:logging(Logfile, lists:concat(["client_", NextNNr, node(), "-", pid_to_list(self()), "- Start: ", werkzeug:timeMilliSecond(), "\n"])),
  Logfile;

getLogFile(Logfile, _NextNNr) -> Logfile.

start_sending(0, Logfile, ReaderNNRs, SendWait, ServerPID) ->
  ServerPID ! {self(), getmsgid},
  receive
    {nid, NextNNr} ->
      werkzeug:logging(Logfile, lists:concat(["EDITOR>>> message number ", NextNNr, " forgotten to send\n"]))
  end,
  NewSendWait = calculateNewInterval(SendWait),
  {ReaderNNRs, NewSendWait, Logfile};

start_sending(Counter, Logfile, ReaderNNrs, SendWait, ServerPID) ->
  ServerPID ! {self(), getmsgid},
  receive
    {nid, NextNNr} ->
      NewLogFile = getLogFile(Logfile, NextNNr),
      NewReaderNNrs = ReaderNNrs ++ [NextNNr],
      Message = createMessage(NextNNr),
      timer:sleep(SendWait),
      ServerPID ! {dropmessage, [NextNNr, Message, erlang:now()]},
      werkzeug:logging(NewLogFile, lists:concat([Message, " was sent\n"])),
      NewCounter = Counter - 1,
      start_sending(NewCounter, NewLogFile, NewReaderNNrs, SendWait, ServerPID)
  end.