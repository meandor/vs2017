-module(server).
-export([start/0, server/3]).

loadConfig() ->
  {ok, Config} = file:consult("./config/server.cfg"),
  Config.

serverLog() ->
  erlang:list_to_atom(lists:concat(["Server@", node(), ".log"])).

% Lädt die Config Datei, CMEM und startet den HBQ Prozess
init() ->
  % Lade configs
  Config = loadConfig(),
  werkzeug:logging(serverLog(), lists:concat(["Server: server.cfg opened\n"])),
  % Starte CMEM
  {ok, RemTime} = werkzeug:get_config_value(clientlifetime, Config),
  CMEM = cmem:initCMEM(RemTime, serverLog()),
% Starte HBQ
  HBQPID = hbq:start(),
  {Config, CMEM, HBQPID}.

server(Config, CMEM, HBQPID) ->
  receive
    {ClientPID, getmsgid} ->
      NNr = cmem:getClientNNr(CMEM, ClientPID),
      ClientPID ! {nid, NNr},
      server(Config, CMEM, HBQPID);
    {ClientPID, getmessages} ->
      NNr = cmem:getClientNNr(CMEM, ClientPID),
      HBQPID ! {self(), {request, deliverMSG, NNr, ClientPID}},
      server(Config, CMEM, HBQPID);
    {dropmessage, [INNr, Msg, TSclientout]} ->
      HBQPID ! {self(), {request, pushHBQ, [INNr, Msg, TSclientout]}};
    terminate ->
      ok
  end.

start() ->
  {Config, CMEM, HBQPID} = init(),
  {ok, ServerName} = werkzeug:get_config_value(servername, Config),
  ServerPID = spawn(?MODULE, server, [Config, CMEM, HBQPID]),
  register(ServerName, ServerPID),
  werkzeug:logging(serverLog(), lists:concat(["Server: Starttime: ", werkzeug:timeMilliSecond(), " with PID", pid_to_list(ServerPID), "\n"])).
