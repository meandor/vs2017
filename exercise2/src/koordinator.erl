%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator).
-export([start/0, init_coordinator/1, initial_state/1, twenty_percent_of/1, update_minimum/2, handle_briefterm/4]).

log(Message) ->
  Logfile = list_to_atom(lists:concat(["Koordinator@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  werkzeug:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

-spec initial_state(map()) -> map().
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
      log(["getsteeringval: ", pid_to_list(From)]),
      initial_state(State);
    {hello, ClientName} ->
      NewState = maps:update(clients, maps:get(clients, State) ++ [ClientName], State),
      log(["hello: ", atom_to_list(ClientName), " #ofclients: ", integer_to_list(length(maps:get(clients, NewState)))]),
      initial_state(NewState);
    reset -> initial_state(maps:update(clients, [], State));
    kill -> ok;
    step -> build_ring(maps:get(config, State), werkzeug:shuffle(maps:get(clients, State)))
  end,
  State.

build_ring(Config, Clients) ->
  set_neighbors(Clients, Clients),
  {ok, Correct} = werkzeug:get_config_value(korrigieren, Config),
  step(Config, Clients, Correct, utils:max_int_value()).

set_neighbors([Middle, Last], [First, Second | _Tail]) ->
  werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>", Middle, " <- ", Last, " -> ", First, " neighbours set \n"])),
  werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>", Last, " <- ", First, " -> ", Second, " neighbours set \n"])),

  Last ! {setneighbors, Middle, First},
  First ! {setneighbors, Last, Second};


set_neighbors([Left, Middle, Right | Tail], Clients) ->
  werkzeug:logging("Koordinator", lists:concat(["Koordinator@chef.log>>", Left, " <- ", Middle, " -> ", Right, " neighbours set \n"])),
  Middle ! {setneighbors, Left, Right},
  set_neighbors([Middle, Right] ++ Tail, Clients).

step(Config, Clients, Toggled, Minimum) ->
  receive
    toggle -> step(Config, Clients, not(Toggled), Minimum);
    prompt -> promt_all_ggt(Clients);
    nudge -> check_status_all_ggt(Clients);
    {calc, WggT} -> startCalculation(WggT, Clients);
    kill -> exit(self(), normal), ok;
    {briefmi, {Clientname, CMi, CZeit}} ->
      log(["Client "  + Clientname + " informs about new Mi " , CMi, " at ", CZeit]),
      NewMinimum = update_minimum(CMi,Minimum),
      step(Config, Clients, Toggled, NewMinimum);
    {From, briefterm, {Clientname, CMi, CZeit}} ->
      if
        Toggled -> handle_briefterm(CMi, Minimum, From, CZeit)
      end
  end
.


handle_briefterm(CMi, Minimum, Client, CZeit) ->
  if
    CMi > Minimum ->
      %TODO Logging
      log(["Client sent false termination message"]),
      Client ! {sendy, Minimum}
  end .

update_minimum(CMi, CurrentMinimum) ->
  min(CMi, CurrentMinimum)
.

startCalculation(WggT, Clients) ->
  ClientsToStart = twenty_percent_of(Clients),
  Mis = werkzeug:bestimme_mis(WggT, length(Clients)),
  sendMisToClients(Mis, ClientsToStart).

sendMisToClients([Mi | Tail], [Client | ClientTail]) ->
  Client ! {sendy, Mi},
  sendMisToClients(Tail, ClientTail)
.

twenty_percent_of(Clients) ->
  TwentyPercent = utils:ceiling(length(Clients) * 0.2),
  lists:nthtail(length(Clients) - TwentyPercent, werkzeug:shuffle(Clients)).




promt_all_ggt([]) -> ok;
promt_all_ggt([Client | RestClients]) ->
  Client ! {self(), tellmi},
  receive
    {mi, Mi} -> log(["client: ", Client, " has mi: ", Mi])
  end,
  promt_all_ggt(RestClients)
.

check_status_all_ggt([]) -> ok;
check_status_all_ggt([Client | RestClients]) ->
  ClientPID = whereis(Client),
  case ClientPID of
    undefined  -> log(["client: ", Client, " is dead"]);
    _Else -> Client ! {self(), pingGGT},
      receive
        {pongGGT, ClientName} -> log(["client: ", ClientName, " is alive"])
      end
  end,
  check_status_all_ggt(RestClients)
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

