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
  receive_loop(Config, [], initial, false).

%%{arbeitszeit, 2}.
%%{termzeit, 42}.
%%{ggtprozessnummer, 9}.
%%{nameservicenode, 'ns@Brummpa'}.
%%{nameservicename, nameservice}.
%%{koordinatorname, chef}.
%%{quote, 80}.
%%{korrigieren, 1}.


receive_loop(Config, Clients, State, Toggled) ->
  receive
    {From, getsteeringval} ->
      {ok, WorkingTime} = werkzeug:get_config_value(arbeitszeit, Config),
      {ok, TerminationTime} = werkzeug:get_config_value(termzeit, Config),
      {ok, Quota} = werkzeug:get_config_value(quote, Config),
      {ok, GGTProcessNumber} = werkzeug:get_config_value(ggtprozessnummer, Config),
      From ! {steeringval,WorkingTime,TerminationTime,Quota,GGTProcessNumber} ;
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

