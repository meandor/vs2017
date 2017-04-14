%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt).
-export([start/6]).


loggingAtom(GGTName) ->
  LogfileName = lists:concat(["GGT@", GGTName, ".log"]),
  erlang:list_to_atom(LogfileName).
start(WorkingTime,TerminationTime,Quota, GGTName, Koordinator) ->
  start(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, "./config/ggt.cfg").

start(WorkingTime,TerminationTime,Quota, GGTName, Koordinator, ConfigPath) ->

  % Der ggT-Prozess meldet sich beim Koordinator mit seinem Namen an (hello) und beim Namensdienst (rebind).
  % Er registriert sich ebenfalls lokal auf der Erlang-Node mit seinem Namen (register).
  % Der ggT-Prozess erwartet dann vom Koordinator die Informationen über seine Nachbarn (setneighbors).

  Config = werkzeug:loadConfig(ConfigPath),
  {ok, NSNode} = werkzeug:get_config_value(nameservicenode, Config),
  {ok, NSName} = werkzeug:get_config_value(nameservicename, Config),
  %net_adm:ping(NSNode),
  Nameservice = global:whereis_name(nameservice),
 % register(GGTName, self())
  Nameservice ! {self(),{rebind,GGTName,node()}},
  Koordinator ! {hello,GGTName}
.
 % receive_loop(WorkingTime, TerminationTime, Quota, GGTName).

receive_loop(WorkingTime,TerminationTime,Quota, GGTName) ->
  receive
    {setneighbors,LeftN,RightN} -> ok;
    {setpm,MiNeu} -> ok;
    {sendy,Y} -> ok;
    {From,{vote,Initiator}} -> ok;
    {voteYes,Name} -> ok;
    {From,tellmi} -> ok;
    {From,pingGGT} -> From ! {pongGGT,GGTName};
    kill -> exit("Kill command received"), ok
  end,
  receive_loop(WorkingTime, TerminationTime, Quota, GGTName)
.

