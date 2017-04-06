-module(server).
-export([start/0, server/6]).

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

server(Config, CMEM, HBQPID, NextNNr, Timer, Latency) ->
  receive
    {ClientPID, getmsgid} ->
      NewTimer = werkzeug:reset_timer(Timer, Latency, terminate),
      ClientPID ! {nid, NextNNr},
      NewNextNNr = NextNNr + 1,
      server(Config, CMEM, HBQPID, NewNextNNr, NewTimer, Latency);
    {ClientPID, getmessages} ->
      NewTimer = werkzeug:reset_timer(Timer, Latency, terminate),
      NNr = cmem:getClientNNr(CMEM, ClientPID),
      HBQPID ! {self(), {request, deliverMSG, NNr, ClientPID}},
      werkzeug:logging(serverLog(), lists:concat(["Server: deliver message ", NNr, " to ", erlang:pid_to_list(ClientPID), "\n"])),
      receive
        {reply, SendNNr} ->
          NewCMEM = cmem:updateClient(CMEM, ClientPID, SendNNr, serverLog()),
          werkzeug:logging(serverLog(), lists:concat(["Server: client ", erlang:pid_to_list(ClientPID), " got message ", SendNNr, "\n"])),
          server(Config, NewCMEM, HBQPID, NextNNr, NewTimer, Latency)
      end;
    {dropmessage, [INNr, Msg, TSclientout]} ->
      NewTimer = werkzeug:reset_timer(Timer, Latency, terminate),
      HBQPID ! {self(), {request, pushHBQ, [INNr, Msg, TSclientout]}},
      werkzeug:logging(serverLog(), lists:concat(["Server: message ", INNr, " was added into the HBQ\n"])),
      server(Config, CMEM, HBQPID, NextNNr, NewTimer, Latency);
    test ->
      NewTimer = werkzeug:reset_timer(Timer, Latency, terminate),
      werkzeug:logging(serverLog(), lists:concat(["test\n"])),
      server(Config, CMEM, HBQPID, NextNNr, NewTimer, Latency);
    terminate ->
      werkzeug:logging(serverLog(), lists:concat(["Server: starting shutdown sequence ", werkzeug:timeMilliSecond(), "\n"])),
      HBQPID ! {self(), {request, dellHBQ}},
      server(Config, CMEM, HBQPID, NextNNr, Timer, Latency);
    {reply, ok} ->
      werkzeug:logging(serverLog(), lists:concat(["Server: shutting down ", werkzeug:timeMilliSecond(), "\n"])),
      ok
  end.

start() ->
  {Config, CMEM, HBQPID} = init(),
  {ok, ServerName} = werkzeug:get_config_value(servername, Config),
  {ok, Latency} = werkzeug:get_config_value(latency, Config),
  {ok, Timer} = timer:send_after(Latency * 1000, ServerName, terminate),
  ServerPID = spawn(?MODULE, server, [Config, CMEM, HBQPID, 1, Timer, Latency]),
  register(ServerName, ServerPID),
  werkzeug:logging(serverLog(), lists:concat(["Server: Starttime: ", werkzeug:timeMilliSecond(), " with PID", pid_to_list(ServerPID), "\n"])).
