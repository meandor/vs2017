%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator).
-export([start/0, init_coordinator/1]).

log(Message) ->
  Logfile = list_to_atom(lists:concat(["Koordinator@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  werkzeug:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

initial(Config, Clients) ->
  receive
    {From, getsteeringval} ->
      {ok, WorkingTime} = werkzeug:get_config_value(arbeitszeit, Config),
      {ok, TerminationTime} = werkzeug:get_config_value(termzeit, Config),
      {ok, Quota} = werkzeug:get_config_value(quote, Config),
      {ok, GGTProcessNumber} = werkzeug:get_config_value(ggtprozessnummer, Config),
      From ! {steeringval, WorkingTime, TerminationTime, Quota, GGTProcessNumber};
    {hello, Clientname} ->
      werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>", Clientname, " added \n"])),
      initial(Config, Clients ++ [Clientname]);
    reset -> initial(Config, []);
    kill -> exit(self(), normal), ok;
    step -> build_ring(Config, werkzeug:shuffle(Clients))
  end,
  initial(Config, Clients).

build_ring(Config, Clients) ->
  set_neighbors(Clients, Clients),
  step(Config, Clients, false).

set_neighbors([Middle, Last], [First, Second | _Tail]) ->
  werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>", Middle, " <- ", Last, " -> ", First, " neighbours set \n"])),
  werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>", Last, " <- ", First, " -> ", Second, " neighbours set \n"])),

  Last ! {setneighbors, Middle, First},
  First ! {setneighbors, Last, Second};


set_neighbors([Left, Middle, Right | Tail], Clients) ->
  werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>", Left, " <- ", Middle, " -> ", Right, " neighbours set \n"])),
  Middle ! {setneighbors, Left, Right},
  set_neighbors([Middle, Right] ++ Tail, Clients).

step(Config, Clients, Toggled) ->
  receive
    toggle -> step(Config, Clients, not(Toggled));
    prompt -> ok;
    nudge -> ok;
    {calc, WggT} -> ok;
    kill -> exit(self(), normal), ok;
    {briefmi, {Clientname, CMi, CZeit}} -> ok;
    {From, briefterm, {Clientname, CMi, CZeit}} -> ok
  end
.

init_coordinator(Config) ->
  log(["starttime: ", werkzeug:timeMilliSecond(), " with PID ", pid_to_list(self())]),
  log(["opening config..."]),
  {ok, CoordName} = werkzeug:get_config_value(koordinatorname, Config),
  log(["loaded config..."]),
  NameService = utils:bind_nameservice(Config),
  log(["Nameservice '", pid_to_list(NameService), "' bound..."]),
  erlang:register(CoordName, self()),
  log(["registered locally..."]),
  NameService ! {self(), {rebind, CoordName, node()}},
  receive
    ok -> log(["registered with nameservice..."])
  end,
  initial(Config, []).

start() -> start("./config/koordinator.cfg").

start(ConfigPath) ->
  Config = werkzeug:loadConfig(ConfigPath),
  spawn(?MODULE, init_coordinator, [Config]).
