%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator).
-export([start/1]).


start(Config) -> ok.

receive_loop(Config) ->
  receive
    {From, getsteeringval} -> ok;
    {hello,Clientname} -> ok;
    {briefmi,{Clientname,CMi,CZeit}} -> ok;
    {From,briefterm,{Clientname,CMi,CZeit}} -> ok;
    reset -> ok;
    step -> ok;
    prompt -> ok;
    nudge -> ok;
    toggle -> ok;
    {calc,WggT} -> ok;
    kill -> ok
  end
.

