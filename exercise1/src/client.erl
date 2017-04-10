-module(client).

-export([startClients/0, startClient/1]).

loadConfig() ->
  {ok, Config} = file:consult("./config/client.cfg"),
  Config.

clientLog() ->
  erlang:list_to_atom(lists:concat(["Client@", node(), ".log"])).

% Recursively call first editor then reader clients until timeout occurs
startClient(Lifetime) ->
  Config = loadConfig(),
  {ok, ServerName} = werkzeug:get_config_value(servername, Config),
  {ok, SendeIntervall} = werkzeug:get_config_value(sendeintervall, Config),
  % Start editor
  editor:start_sending(5, clientLog(), [], SendeIntervall, ServerName),
  % Start timeout handler
  timer:kill_after(Lifetime),
  self().

do_times(Times, Fn, Arg1, Arg2, Arg3) -> do_times(Times, Fn, Arg1, Arg2, Arg3, []).
do_times(0, _Fn, _Arg1, _Arg2, _Arg3, Results) -> Results;
do_times(Times, Fn, Arg1, Arg2, Arg3, Results) ->
  NewResults = Results ++ [Fn(Arg1, Arg2, Arg3)],
  NewTimes = Times - 1,
  do_times(NewTimes, Fn, Arg1, Arg2, Arg3, NewResults).

startClients() ->
  Config = loadConfig(),
  {ok, Lifetime} = werkzeug:get_config_value(lifetime, Config),
  {ok, Clients} = werkzeug:get_config_value(clients, Config),
  erlang:spawn(?MODULE, startClient, [Lifetime]),
  do_times(Clients, fun erlang:spawn/3, ?MODULE, startClient, [Lifetime]).