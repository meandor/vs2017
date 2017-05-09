%%%-------------------------------------------------------------------
%%% @doc
%%% Used for utils functions
%%% @end
%%%-------------------------------------------------------------------
-module(utils).

-export([bind_nameservice/1, ceiling/1, max_int_value/0]).

bind_nameservice(Config) ->
  {ok, NSNode} = werkzeug:get_config_value(nameservicenode, Config),
  {ok, NSName} = werkzeug:get_config_value(nameservicename, Config),
  pong = net_adm:ping(NSNode),
  NameService = global:whereis_name(NSName),
  NameService.

max_int_value() -> 134217728.

ceiling(X) when X < 0 ->
  trunc(X);
ceiling(X) ->
  T = trunc(X),
  case X - T == 0 of
    true -> T;
    false -> T + 1
  end.