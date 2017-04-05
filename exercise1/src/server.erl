-module(server).
-export([start/0, server/4]).

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

server(Config, CMEM, HBQPID, NextNNr) ->
  receive
    {ClientPID, getmsgid} ->
      ClientPID ! {nid, NextNNr},
      NewNextNNr = NextNNr + 1,
      server(Config, CMEM, HBQPID, NewNextNNr);
    {ClientPID, getmessages} ->
      NNr = cmem:getClientNNr(CMEM, ClientPID),
      HBQPID ! {self(), {request, deliverMSG, NNr, ClientPID}},
      receive
        {reply, SendNNr} ->
          NewCMEM = cmem:updateClient(CMEM, ClientPID, SendNNr, serverLog()),
          server(Config, NewCMEM, HBQPID, NextNNr)
      end;
    {dropmessage, [INNr, Msg, TSclientout]} ->
      HBQPID ! {self(), {request, pushHBQ, [INNr, Msg, TSclientout]}};
    terminate ->
      ok
  end.

start() ->
  {Config, CMEM, HBQPID} = init(),
  {ok, ServerName} = werkzeug:get_config_value(servername, Config),
  ServerPID = spawn(?MODULE, server, [Config, CMEM, HBQPID, 1]),
  register(ServerName, ServerPID),
  werkzeug:logging(serverLog(), lists:concat(["Server: Starttime: ", werkzeug:timeMilliSecond(), " with PID", pid_to_list(ServerPID), "\n"])).
