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

calculation_state(Config, Clients, Toggled, Minimum) ->
  receive
    toggle ->
      log(["Correct flag is now set to: ", not(Toggled)]),
      calculation_state(Config, Clients, not(Toggled), Minimum);
    prompt -> promt_all_ggt(Clients);
    nudge -> check_status_all_ggt(Clients);
    {calc, WggT} -> startCalculation(WggT, Clients);
    kill -> shutdown(#{config => Config, clients=> Clients});
    {briefmi, {Clientname, CMi, CZeit}} ->
      log([Clientname, " reports new Mi ", CMi, " at ", werkzeug:now2string(CZeit)]),
      NewMinimum = update_minimum(CMi, Minimum),
      calculation_state(Config, Clients, Toggled, NewMinimum);
    {From, briefterm, {Clientname, CMi, CTermZeit}} ->
      if
        Toggled =:= true -> handle_briefterm(CMi, Minimum, From, CTermZeit);
        true ->
          log([atom_to_list(Clientname), " reports termination with ggT ", CMi, " at ", werkzeug:now2string(CTermZeit)])
      end
  end,
  calculation_state(Config, Clients, Toggled, Minimum)
.

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

startCalculation(WggT, Clients) ->
  ClientsToStart = twenty_percent_of(Clients),
  SetPMMis = werkzeug:bestimme_mis(WggT, length(Clients)),
  StartMis = werkzeug:bestimme_mis(WggT, length(ClientsToStart)),
  set_initial_mis(SetPMMis, Clients),
  sendMisToClients(StartMis, ClientsToStart).

set_initial_mis([], []) -> [];
set_initial_mis([Mi | Tail], [Client | ClientTail]) ->
  Client ! {setpm, Mi},
  set_initial_mis(Tail, ClientTail)
.


sendMisToClients([], []) -> [];
sendMisToClients([Mi | Tail], [Client | ClientTail]) ->
  Client ! {sendy, Mi},
  sendMisToClients(Tail, ClientTail)
.

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
  check_status_all_ggt(RestClients)
.

set_neighbors(NameService, [Middle, Last], [First, Second | _Tail]) ->
  NameService ! {self(), {lookup, Last}},
  receive
    not_found ->
      log(["Could not bind ", atom_to_list(Last)]);
    {pin, LastPID} ->
      log(["Bound ggT-process ", atom_to_list(Last), " with neighbours: ", atom_to_list(Middle), " ", atom_to_list(First)]),
      LastPID ! {setneighbors, Middle, First}
  end,

  NameService ! {self(), {lookup, First}},
  receive
    not_found ->
      log(["Could not bind ", atom_to_list(First)]);
    {pin, FirstPID} ->
      log(["Bound ggT-process ", atom_to_list(First), " with neighbours: ", atom_to_list(Last), " ", atom_to_list(Second)]),
      FirstPID ! {setneighbors, Last, Second}
  end;

set_neighbors(NameService, [Left, Middle, Right | Tail], Clients) ->
  NameService ! {self(), {lookup, Middle}},
  receive
    not_found ->
      log(["Could not bind ", atom_to_list(Middle)]);
    {pin, MiddlePID} ->
      log(["Bound ggT-process ", atom_to_list(Middle), " with neighbours: ", atom_to_list(Left), " ", atom_to_list(Right)]),
      MiddlePID ! {setneighbors, Left, Right}
  end,
  set_neighbors(NameService, [Middle, Right] ++ Tail, Clients).

transition_to_calculation_state(State) ->
  Config = maps:get(config, State),
  {ok, ExpectedClients} = werkzeug:get_config_value(ggtprozessnummer, Config),
  ActualClients = length(maps:get(clients, State)),
  log(["Initial state completed. Registered ", integer_to_list(ExpectedClients), "/", integer_to_list(ActualClients), " ggT-processes"]),
  log(["Start building ring"]),
  NameService = utils:bind_nameservice(Config),
  % build ring
  ShuffledClients = werkzeug:shuffle(maps:get(clients, State)),
  set_neighbors(NameService, ShuffledClients, ShuffledClients),
  log(["Done building ring"]),
  log(["Switching to calculation state"]),
  {ok, Correct} = werkzeug:get_config_value(korrigieren, Config),
  case Correct of
    1 -> calculation_state(Config, maps:get(clients, State), true, utils:max_int_value());
    _Else -> calculation_state(Config, maps:get(clients, State), false, utils:max_int_value())
  end
.


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
  State = #{config => Config, clients => []},
  initial_state(State).

start() ->
  erlang:set_cookie(koordinator, vsp),
  start("./config/koordinator.cfg").

start(ConfigPath) ->
  Config = werkzeug:loadConfig(ConfigPath),
  spawn(?MODULE, init_coordinator, [Config]).
