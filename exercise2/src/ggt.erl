%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt).
-export([start/4]).

start(WorkingTime,TerminationTime,Quota, GGTName) ->
  % Der ggT-Prozess meldet sich beim Koordinator mit seinem Namen an (hello) und beim Namensdienst (rebind).
  % Er registriert sich ebenfalls lokal auf der Erlang-Node mit seinem Namen (register).
  % Der ggT-Prozess erwartet dann vom Koordinator die Informationen über seine Nachbarn (setneighbors).
  receive_loop(WorkingTime, TerminationTime, Quota, GGTName).

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
  end
.

