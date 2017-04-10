-module(client).

-export([startClients/0, startClient/1]).

loadConfig() ->
  {ok, Config} = file:consult("./config/client.cfg"),
  Config.

clientLog() ->
  erlang:list_to_atom(lists:concat(["client_nummer@", node(), ".log"])).

startClient(ServerPID) ->
  Config = loadConfig(),
  {ok, Lifetime} = werkzeug:get_config_value(lifetime, Config),
  {ok, SendeIntervall} = werkzeug:get_config_value(sendeintervall, Config),
  % Start editor
  ClientPID = erlang:spawn(editor, start_sending, [5, clientLog(), [], SendeIntervall, ServerPID]),
  % Start timeout handler
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
  do_times(Clients, fun client:startClient/1, ServerName).