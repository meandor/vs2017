-module(client).

-export([startClients/0, startClient/1]).

loadConfig() ->
  {ok, Config} = file:consult("./config/client.cfg"),
  Config.

startClient(Lifetime) ->
  ok.

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
