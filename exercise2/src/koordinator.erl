%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator).
-export([start/1, start/0, receive_loop/2]).


start() -> start("./config/koordinator.cfg").

start(Config) ->  spawn(?MODULE, receive_loop, [Config, chef]).

receive_loop(Config, RegisterName) ->
  werkzeug:register_safe(RegisterName, self()),
  receive_loop(Config, [], initial, false).


receive_loop(Config, Clients, State, Toggled) ->
  receive
    {From, getsteeringval} -> ok;
    {hello,Clientname} -> ok;
    {briefmi,{Clientname,CMi,CZeit}} -> ok;
    {From,briefterm,{Clientname,CMi,CZeit}} -> ok;
    reset -> receive_loop(Config, [], initial, false);
    step -> receive_loop(Config, Clients, ready, Toggled);
    prompt -> ok;
    nudge -> ok;
    toggle -> receive_loop(Config, Clients, State, not(Toggled));
    {calc,WggT} -> ok;
    kill -> ok
  end,
  receive_loop(Config, Clients, State, Toggled)
.

