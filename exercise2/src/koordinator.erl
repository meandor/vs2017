%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator).
-export([start/1, start/0, receive_loop/1]).


start() -> start("./config/koordinator.cfg").

start(Config) -> register(chef,self()),  spawn(?MODULE, receive_loop, [Config]).

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
  end,
  receive_loop(Config)
.

