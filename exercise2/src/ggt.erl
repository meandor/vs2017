%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt).
-export([start/6, receive_loop/5]).


loggingAtom(GGTName) ->
  LogfileName = lists:concat(["GGT@", GGTName, ".log"]),
  erlang:list_to_atom(LogfileName).

start(WorkingTime,TerminationTime,Quota, GGTName, Koordinator, Nameservice) ->
  Koordinator ! {hello,GGTName},
  % Er registriert sich ebenfalls lokal auf der Erlang-Node mit seinem Namen (register).
  % Der ggT-Prozess erwartet dann vom Koordinator die Informationen über seine Nachbarn (setneighbors).
  spawn(?MODULE, receive_loop, [WorkingTime, TerminationTime, Quota, GGTName, Nameservice]).

receive_loop(WorkingTime,TerminationTime,Quota, GGTName, Nameservice) ->
  Nameservice ! {self(),{rebind,GGTName,node()}},
  Registered = erlang:whereis(GGTName) ,
  if
    Registered == undefined -> register(GGTName ,self())
  end,
  receive_loop(WorkingTime, TerminationTime, Quota, GGTName).

receive_loop(WorkingTime,TerminationTime,Quota, GGTName) ->
  werkzeug:logging(lists:concat([GGTName, "@vsp"]), "GGT in receive loop\n"),
  receive
    {setneighbors,LeftN,RightN} -> ok;
    {setpm,MiNeu} -> ok;
    {sendy,Y} -> ok;
    {From,{vote,Initiator}} -> ok;
    {voteYes,Name} -> ok;
    {From,tellmi} -> ok;
    {From,pingGGT} -> From ! {pongGGT,GGTName};
    kill -> werkzeug:logging(lists:concat([GGTName, "@vsp"]), "Kill received"), exit(self(), normal), ok
  end,
  receive_loop(WorkingTime, TerminationTime, Quota, GGTName)
.

