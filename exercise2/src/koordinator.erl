%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator).
-export([start/0, init_coordinator/1, initial_state/1, twenty_percent_of/1, update_minimum/2, handle_briefterm/4, set_initial_mis/2]).

log(Message) ->
  Logfile = list_to_atom(lists:concat(["Koordinator@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  werkzeug:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

step(Config, Clients, Toggled, Minimum) ->
  receive
    toggle -> step(Config, Clients, not(Toggled), Minimum);
    prompt -> promt_all_ggt(Clients);
    nudge -> check_status_all_ggt(Clients);
    {calc, WggT} -> startCalculation(WggT, Clients);
    kill -> shutdown(#{config => Config, clients=> Clients});
    {briefmi, {Clientname, CMi, CZeit}} ->
      log(["Client " + Clientname + " informs about new Mi ", CMi, " at ", CZeit]),
      NewMinimum = update_minimum(CMi, Minimum),
      step(Config, Clients, Toggled, NewMinimum);
    {From, briefterm, {Clientname, CMi, CZeit}} ->
      if
        Toggled -> handle_briefterm(CMi, Minimum, From, CZeit);
        true -> log(["Client", Clientname, " reports result ", CMi])
      end
  end
.

handle_briefterm(CMi, Minimum, Client, CZeit) ->
  if
    CMi > Minimum ->
      %TODO Logging
      %log(["Client ", Client, " sent false termination message"]),
      Client ! {sendy, Minimum}
  end
.

update_minimum(CMi, CurrentMinimum) ->
  min(CMi, CurrentMinimum)
.

startCalculation(WggT, Clients) ->
  ClientsToStart = twenty_percent_of(Clients),
  SetPMMis = werkzeug:bestimme_mis(WggT, length(ClientsToStart)),
  StartMis = lower_by_ggt(SetPMMis, WggT),
  set_initial_mis(SetPMMis, ClientsToStart),
  sendMisToClients(StartMis, ClientsToStart).

lower_by_ggt([], _Ggt) -> [];
lower_by_ggt([Mi | Tail], GGt) ->
  lists:append([Mi - GGt * 20], lower_by_ggt(Tail, GGt)).

set_initial_mis([], []) -> ok;
set_initial_mis([Mi | Tail], [Client | ClientTail]) ->
  Client ! {setpm, Mi},
  set_initial_mis(Tail, ClientTail)
.


sendMisToClients([], []) -> ok;
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
    undefined -> log(["client: ", Client, " is dead"]);
    _Else -> Client ! {self(), pingGGT},
      receive
        {pongGGT, ClientName} -> log(["client: ", ClientName, " is alive"])
      end
  end,
  check_status_all_ggt(RestClients)
.

set_neighbors([Middle, Last], [First, Second | _Tail]) ->
  Last ! {setneighbors, Middle, First},
  First ! {setneighbors, Last, Second};


set_neighbors([Left, Middle, Right | Tail], Clients) ->
  Middle ! {setneighbors, Left, Right},
  set_neighbors([Middle, Right] ++ Tail, Clients).

build_ring(Config, Clients) ->
  set_neighbors(Clients, Clients),
  {ok, Correct} = werkzeug:get_config_value(korrigieren, Config),
  step(Config, Clients, Correct, utils:max_int_value()).

kill_clients([]) -> ok;
kill_clients([Client | Tail]) ->
  WhereIs = whereis(Client),
  case WhereIs of
    undefined -> ok;
    _Else -> Client ! kill
  end,
  kill_clients(Tail).

shutdown(State) ->
  log(["Shutting down ", integer_to_list(length(maps:get(clients, State))), " ggT-processes"]),
  kill_clients(maps:get(clients, State)),
  exit(self(), normal), ok.

-spec initial_state(map()) -> map().
initial_state(State) ->
  receive
    {From, getsteeringval} ->
      Config = maps:get(config, State),
      {ok, WorkingTime} = werkzeug:get_config_value(arbeitszeit, Config),
      {ok, TerminationTime} = werkzeug:get_config_value(termzeit, Config),
      {ok, QuotaPercentage} = werkzeug:get_config_value(quote, Config),
      {ok, GGTProcessNumber} = werkzeug:get_config_value(ggtprozessnummer, Config),
      Quota = max(2, round(length(maps:get(clients, State)) * QuotaPercentage / 100)),
      From ! {steeringval, WorkingTime, TerminationTime, Quota, GGTProcessNumber},
      log(["getsteeringval: ", pid_to_list(From)]),
      initial_state(State);
    {hello, ClientName} ->
      NewState = maps:update(clients, maps:get(clients, State) ++ [ClientName], State),
      log(["hello: ", atom_to_list(ClientName), " #ofclients: ", integer_to_list(length(maps:get(clients, NewState)))]),
      initial_state(NewState);
    reset -> initial_state(maps:update(clients, [], State));
    kill -> shutdown(State);
    step -> build_ring(maps:get(config, State), werkzeug:shuffle(maps:get(clients, State)))
  end,
  State.

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
