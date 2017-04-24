%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator).
-export([start/1, start/0, receive_loop/2]).


start() -> start("./config/koordinator.cfg").

start(Config) ->
  ConfigDict = werkzeug:loadConfig(Config),
  {ok, KoordinatorName} = werkzeug:get_config_value(koordinatorname, ConfigDict),
  spawn(?MODULE, receive_loop, [ConfigDict, KoordinatorName]).

receive_loop(Config, RegisterName) ->
  werkzeug:register_safe(RegisterName, self()),
  initial(Config, []).

initial(Config, Clients) ->

  receive
    {From, getsteeringval} ->
      {ok, WorkingTime} = werkzeug:get_config_value(arbeitszeit, Config),
      {ok, TerminationTime} = werkzeug:get_config_value(termzeit, Config),
      {ok, Quota} = werkzeug:get_config_value(quote, Config),
      {ok, GGTProcessNumber} = werkzeug:get_config_value(ggtprozessnummer, Config),
      From ! {steeringval,WorkingTime,TerminationTime,Quota,GGTProcessNumber} ;

    {hello,Clientname} ->
      % Generate some randomness in the ring
      werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>" , Clientname, " added \n"])),
      LeftOrRight = bool_rand(),
      case LeftOrRight of
        _Any -> initial(Config, Clients ++ [Clientname]);
        true -> initial(Config, Clients ++ [Clientname]);
        false -> initial(Config, [Clientname] ++ Clients)
      end;

    reset -> initial(Config, []);
    kill -> exit(self(), normal), ok;
    step -> build_ring(Config, Clients)
  end,
  initial(Config, Clients)
.

build_ring(Config, Clients) ->
  set_neighbors(Clients, Clients),
  step(Config, Clients, false).

set_neighbors([Middle, Last], [First, Second | _Tail]) ->
  werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>" , Middle, " <- ",  Last,  " -> ", First, " neighbours set \n"])),
  werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>" , Last, " <- ",  First,  " -> ", Second, " neighbours set \n"])),

  Last ! {setneighbors, Middle, First},
  First! {setneighbors, Last, Second};


set_neighbors([Left, Middle, Right | Tail], Clients) ->
  werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>" , Left, " <- ",  Middle,  " -> ", Right, " neighbours set \n"])),
  Middle ! {setneighbors, Left, Right},
  set_neighbors([Middle, Right] ++ Tail, Clients).

step(Config, Clients, Toggled) ->
  receive
    toggle -> step(Config, Clients, not(Toggled));
    prompt -> ok;
    nudge -> ok;
    {calc,WggT} -> ok;
    kill -> exit(self(), normal), ok;
    {briefmi,{Clientname,CMi,CZeit}} -> ok;
    {From,briefterm,{Clientname,CMi,CZeit}} -> ok
  end
.



bool_rand() ->
  random:uniform(2) == 1.
