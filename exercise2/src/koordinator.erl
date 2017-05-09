%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(koordinator).
-export([start/0, init_coordinator/1, initial_state/1, twenty_percent_of/1, update_minimum/2, handle_briefterm/4, set_initial_mis/2, transition_to_calculation_state/1]).

log(Message) ->
  Logfile = list_to_atom(lists:concat(["Koordinator@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  werkzeug:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

handle_briefterm(CMi, Minimum, Clientname, CZeit) ->
  if
    CMi > Minimum ->
      log([atom_to_list(Clientname), " reports false termination with ggT ", CMi, " at ", werkzeug:now2string(CZeit)]),
      Clientname ! {sendy, Minimum};
    true ->
      log([atom_to_list(Clientname), " reports correct termination with ggT ", CMi, " at ", werkzeug:now2string(CZeit)])
  end
.

update_minimum(CMi, CurrentMinimum) ->
  min(CMi, CurrentMinimum)
.

sendMisToClients([], []) -> [];
sendMisToClients([Mi | Tail], [Client | ClientTail]) ->
  Client ! {sendy, Mi},
  sendMisToClients(Tail, ClientTail).

twenty_percent_of(Clients) ->
  TwentyPercent = utils:ceiling(length(Clients) * 0.2),
  lists:nthtail(length(Clients) - TwentyPercent, werkzeug:shuffle(Clients)).

promt_all_ggt([]) -> [];
promt_all_ggt([Client | RestClients]) ->
  Client ! {self(), tellmi},
  receive
    {mi, Mi} -> log(["client: ", Client, " has mi: ", Mi])
  end,
  promt_all_ggt(RestClients)
.

check_status_all_ggt([]) -> [];
check_status_all_ggt([Client | RestClients]) ->
  ClientPID = whereis(Client),
  case ClientPID of
    undefined -> log(["client: ", Client, " is dead"]);
    _Else -> Client ! {self(), pingGGT},
      receive
        {pongGGT, ClientName} -> log(["client: ", ClientName, " is alive"])
      end
  end,
  check_status_all_ggt(RestClients).

set_initial_mis([], []) -> ok;
set_initial_mis([Mi | Tail], [Client | ClientTail]) ->
  Client ! {setpm, Mi},
  set_initial_mis(Tail, ClientTail).

-spec startCalculation(integer(), list()) -> any().
startCalculation(WggT, Clients) ->
  log(["Start calculation for ggT: ", integer_to_list(WggT)]),
  Mis = werkzeug:bestimme_mis(WggT, length(Clients)),
  set_initial_mis(Mis, Clients),
  StartingClients = twenty_percent_of(Clients),
  StartMis = werkzeug:bestimme_mis(WggT, length(StartingClients)),
  sendMisToClients(StartMis, StartingClients).

calculation_state(State) ->
  receive
    {calc, WggT} ->
      startCalculation(WggT, maps:get(clients, State));
%%    toggle ->
%%      log(["Correct flag is now set to: ", not(Toggled)]),
%%      calculation_state(Config, Clients, not(Toggled), Minimum);
%%    prompt -> promt_all_ggt(Clients);
%%    nudge -> check_status_all_ggt(Clients);
    kill -> shutdown(State)
%%    {briefmi, {Clientname, CMi, CZeit}} ->
%%      log([Clientname, " reports new Mi ", CMi, " at ", werkzeug:now2string(CZeit)]),
%%      NewMinimum = update_minimum(CMi, Minimum),
%%      calculation_state(Config, Clients, Toggled, NewMinimum);
%%    {From, briefterm, {Clientname, CMi, CTermZeit}} ->
%%      if
%%        Toggled =:= true -> handle_briefterm(CMi, Minimum, From, CTermZeit);
%%        true ->
%%          log([atom_to_list(Clientname), " reports termination with ggT ", CMi, " at ", werkzeug:now2string(CTermZeit)])
%%      end
  end,
  calculation_state(State).

set_neighbors(ClientsToPID, [Middle, Last], [First, Second | _Tail]) ->
  log(["Set neighbour for ggT-process ", atom_to_list(Last), " with neighbours: ", atom_to_list(Middle), " ", atom_to_list(First)]),
  maps:get(Last, ClientsToPID) ! {setneighbors, Middle, First},
  log(["Set neighbour for ggT-process ", atom_to_list(First), " with neighbours: ", atom_to_list(Last), " ", atom_to_list(Second)]),
  maps:get(First, ClientsToPID) ! {setneighbors, Last, Second};

set_neighbors(ClientsToPID, [Left, Middle, Right | Tail], Clients) ->
  log(["Set neighbour for ggT-process ", atom_to_list(Middle), " with neighbours: ", atom_to_list(Left), " ", atom_to_list(Right)]),
  maps:get(Middle, ClientsToPID) ! {setneighbors, Left, Right},
  set_neighbors(ClientsToPID, [Middle, Right] ++ Tail, Clients).

bind_ggT(NameService, GgTName, ClientsToPID) ->
  NameService ! {self(), {lookup, GgTName}},
  receive
    not_found ->
      log(["Could not bind ", atom_to_list(GgTName)]),
      maps:put(GgTName, undefined, ClientsToPID);
    {pin, GgTPID} ->
      log(["Bound ggT-process ", atom_to_list(GgTName)]),
      maps:put(GgTName, GgTPID, ClientsToPID)
  end.

bind_ggTs(State) ->
  NameService = utils:bind_nameservice(maps:get(config, State)),
  ClientsToPID = lists:foldr(fun(GgTName, Acc) ->
    bind_ggT(NameService, GgTName, Acc) end, maps:get(clientsToPID, State), maps:get(clients, State)),
  maps:update(clientsToPID, ClientsToPID, State).

transition_to_calculation_state(State) ->
  Config = maps:get(config, State),
  {ok, ExpectedClients} = werkzeug:get_config_value(ggtprozessnummer, Config),
  ActualClients = length(maps:get(clients, State)),
  log(["Initial state completed. Registered ", integer_to_list(ExpectedClients), "/", integer_to_list(ActualClients), " ggT-processes"]),
  log(["Start building ring"]),
  BoundClientsState = bind_ggTs(State),
  % build ring
  ShuffledClients = werkzeug:shuffle(maps:get(clients, State)),
  set_neighbors(maps:get(clientsToPID, BoundClientsState), ShuffledClients, ShuffledClients),
  log(["Done building ring"]),
  log(["Switching to calculation state"]),
  calculation_state(BoundClientsState).


kill_clients([]) -> exit(self(), normal), ok;
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
    step -> transition_to_calculation_state(State)
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
  State = #{config => Config, clients => [], clientsToPID => #{}, smallestGgT => utils:max_int_value()},
  initial_state(State).

start() ->
  erlang:set_cookie(koordinator, vsp),
  start("./config/koordinator.cfg").

start(ConfigPath) ->
  Config = werkzeug:loadConfig(ConfigPath),
  spawn(?MODULE, init_coordinator, [Config]).
