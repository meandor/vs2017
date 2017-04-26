%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(utils).

%% API
-export([bind_nameservice/1]).

bind_nameservice(Config) ->
  {ok, NSNode} = werkzeug:get_config_value(nameservicenode, Config),
  {ok, NSName} = werkzeug:get_config_value(nameservicename, Config),
  pong = net_adm:ping(NSNode),
  NameService = global:whereis_name(NSName),
  NameService.
