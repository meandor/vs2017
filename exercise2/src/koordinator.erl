%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator).
-export([start/0, init_coordinator/1, initial_state/1]).

log(Message) ->
  Logfile = list_to_atom(lists:concat(["Koordinator@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  werkzeug:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

-spec initial_state(map()) -> any().
initial_state(State) ->
  receive
    {From, getsteeringval} ->
      Config = maps:get(config, State),
      {ok, WorkingTime} = werkzeug:get_config_value(arbeitszeit, Config),
      {ok, TerminationTime} = werkzeug:get_config_value(termzeit, Config),
      {ok, QuotaPercentage} = werkzeug:get_config_value(quote, Config),
      {ok, GGTProcessNumber} = werkzeug:get_config_value(ggtprozessnummer, Config),
      Quota = max(2, round(GGTProcessNumber * QuotaPercentage / 100)),
      From ! {steeringval, WorkingTime, TerminationTime, Quota, GGTProcessNumber},
      initial_state(State);
    {hello, ClientName} ->
      werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>", ClientName, " added \n"])),
      initial_state(maps:update_with(clients, fun(OldList) -> OldList ++ [ClientName] end, State));
    reset -> initial_state(maps:update(clients, [], State));
    kill -> exit(self(), normal), ok;
    step -> build_ring(maps:get(config, State), werkzeug:shuffle(maps:get(clients, State)))
  end.

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
  State = #{config => Config, clients => []},
  initial_state(State).

start() -> start("./config/koordinator.cfg").

start(ConfigPath) ->
  Config = werkzeug:loadConfig(ConfigPath),
  spawn(?MODULE, init_coordinator, [Config]).
