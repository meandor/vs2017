-module(client).

-export([startClients/0, startClient/2]).

loadConfig() ->
  {ok, Config} = file:consult("./config/client.cfg"),
  Config.

clientLog() ->
  erlang:list_to_atom(lists:concat(["Client@", node(), ".log"])).

% Exit client process after Lifetime
startClient(Lifetime, ServerPID) ->
  timer:apply_after(Lifetime, erlang, exit, ["Client Timeout"]), start(ServerPID).

% Recursively call first editor then reader clients until timeout occurs
start(ServerPID) ->
  Config = loadConfig(),
  {ok, ServerName} = werkzeug:get_config_value(servername, Config),
  {ok, SendeIntervall} = werkzeug:get_config_value(sendeintervall, Config),
  startLoop(ServerPID, SendeIntervall * 1000)
  .

startLoop(ServerPID, Intervall) ->
  editor:start(clientLog(), Intervall, ServerPID, self()),
  receive
    {doneSending, ReaderNNrs} -> reader:start_reading(false, clientLog(), ReaderNNrs, ServerPID)
  end,
  startLoop(ServerPID, calculateNewInterval(Intervall)).

do_times(Times, Fn, Arg) -> do_times(Times, Fn, Arg, []).

do_times(0, _Fn, _Arg, Results) -> Results;
do_times(Times, Fn, Arg, Results) ->
  NewResults = Results ++ [Fn(Arg)],
  NewTimes = Times - 1,
  do_times(NewTimes, Fn, Arg, NewResults).

startClients() ->
  Config = loadConfig(),
  {ok, Lifetime} = werkzeug:get_config_value(lifetime, Config),
  {ok, Clients} = werkzeug:get_config_value(clients, Config),
  do_times(Clients, fun client:startClient/1, Lifetime).

calculateNewInterval(Interval) ->
  Increase = werkzeug:bool_rand(),
  Value = trunc(max(Interval * 0.5, 1000)),
  if
    Increase ->
      Interval + Value;
    true ->
      max(Interval - Value, 1000)
  end.