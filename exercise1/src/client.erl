-module(client).

-export([startClients/0, spawnClient/1, startClient/4]).

loadConfig() ->
  {ok, Config} = file:consult("./config/client.cfg"),
  Config.

startClient(ServerPID, Logfile, ReaderNNrs, SendWait) ->
  % Start editor
  {NewReaderNNrs, NewSendWait, NewLogFile} = editor:start_sending(5, Logfile, ReaderNNrs, SendWait, ServerPID),
  % Start reader
  reader:start_reading(false, NewLogFile, NewReaderNNrs, ServerPID),
  % Do everything again
  startClient(ServerPID, NewLogFile, NewReaderNNrs, NewSendWait).

spawnClient(ServerPID) ->
  Config = loadConfig(),
  {ok, Lifetime} = werkzeug:get_config_value(lifetime, Config),
  {ok, SendIntervall} = werkzeug:get_config_value(sendeintervall, Config),
  ClientPID = erlang:spawn(?MODULE, startClient, [ServerPID, [], [], SendIntervall]),
  timer:kill_after(Lifetime, ClientPID),
  ClientPID.

do_times(Times, Fn, Arg) -> do_times(Times, Fn, Arg, []).
do_times(0, _Fn, _Arg, Results) -> Results;
do_times(Times, Fn, Arg, Results) ->
  NewResults = Results ++ [Fn(Arg)],
  NewTimes = Times - 1,
  do_times(NewTimes, Fn, Arg, NewResults).

startClients() ->
  Config = loadConfig(),
  {ok, Clients} = werkzeug:get_config_value(clients, Config),
  {ok, ServerName} = werkzeug:get_config_value(servername, Config),
  do_times(Clients, fun client:spawnClient/1, ServerName).