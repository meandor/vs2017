-module(editor).
-export([start_sending/5, receiveNNr/3, receiveLastNNr/1, createMessage/1]).

calculateNewInterval(Interval) ->
  Increase = werkzeug:bool_rand(),
  Value = trunc(max(Interval * 0.5, 1000)),
  if
    Increase ->
      Interval + Value;
    true ->
      max(Interval - Value, 1000)
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
  werkzeug:logging(Logfile, lists:concat(["client_", NextNNr, node(), "-", pid_to_list(self()), "- Start: ", werkzeug:timeMilliSecond()])),
  Logfile;

getLogFile(Logfile, _NextNNr) -> Logfile.

start_sending(0, Logfile, ReaderNNRs, SendWait, ServerPID) ->
  timer:sleep(SendWait),
  ServerPID ! {self, getmsgid},
  receive
    {nid, NextNNr} -> werkzeug:logging(Logfile, lists:concat(["EDITOR>>> message number ", NextNNr, " forgotten\n"]))
  end,
  ReaderNNRs
;


start_sending(Counter, Logfile, ReaderNNrs, SendWait, ServerPID) ->
  ServerPID ! {self(), getmsgid},
  receive
    {nid, NextNNr} ->
      NewLogFile = getLogFile(Logfile, NextNNr),
      NewReaderNNrs = ReaderNNrs ++ [NextNNr],
      %TODO Message format
      wk ! {dropmessage, [NextNNr, "Nachricht vom Editor", erlang:now()]},
      start_sending(Counter - 1, Logfile, NewReaderNNrs, SendWait, ServerPID)
  end,
  timer:sleep(SendWait),
  werkzeug:logging(Logfile, lists:concat(["EDITOR>> > ", Counter, " ID reqeusted\n"])).