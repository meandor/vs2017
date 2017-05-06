%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt).
-export([start/6, start_ggt_process/1, update_mi_state/1, maybe_update_mi/2, update_neighbours/3]).

-spec log(map(), list()) -> atom().
log(Config, Message) ->
  GgTName = maps:get(ggtname, Config),
  Logfile = list_to_atom(lists:concat(["GGTP_", atom_to_list(GgTName), "@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  werkzeug:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

start_ggt_process(State) ->
  GgTName = maps:get(ggtname, State),
  Coordinator = maps:get(coordinator, State),
  log(State, [atom_to_list(GgTName), " starttime: ", werkzeug:timeMilliSecond(), " with PID ", pid_to_list(self()), " on ", atom_to_list(node())]),
  register(GgTName, self()),
  log(State, ["registered locally"]),
  maps:get(nameservice, State) ! {self(), {rebind, GgTName, node()}},
  receive
    ok -> log(State, ["registered at nameservice"])
  end,
  Coordinator ! {hello, GgTName},
  log(State, ["registered at coordinator"]),
  receive_loop(State).

update_mi_state(State) ->
  maps:update(lastMiUpdate, erlang:now(), State).

update_neighbours(Left, Right, State) ->
  log(State, ["left neighbour registered: ", atom_to_list(Left)]),
  log(State, ["right neighbour registered: ", atom_to_list(Right)]),
  UpdatedNeighborMaps = maps:update(rightneigbor, Right, maps:update(leftneighbor, Left, State)),
  update_mi_state(UpdatedNeighborMaps).

maybe_update_mi(Y, State) ->
  Mi = maps:get(mi, State),
  L = maps:get(mi, State),
  R = maps:get(mi, State),
  Koordinator = maps:get(coordinator, State),
  GGTName = maps:get(mi, State),
  if
    Y < Mi ->
      %timer:sleep(maps:get(workingtime, State)),
      NewMi = ((Mi - 1) rem Y) + 1,
      %log(State, ["mi: ", Y, ", Old mi:", Mi, " NEW MI:", NewMi]),
      %L ! {sendy, NewMi},
      %R ! {sendy, NewMi},
      Koordinator ! {briefmi, {GGTName, NewMi, erlang:now()}},
      maps:update(mi, NewMi, State);
    true -> State
  end.

receive_loop(State) ->
  receive
  % Sets the right and left neighbour processes for the recursive algorithm
    {setneighbors, LeftNeighbour, RightNeighbour} ->
      receive_loop(update_neighbours(LeftNeighbour, RightNeighbour, State));
  % Sets a new Mi to calculate
    {setpm, NewMi} ->
      receive_loop(update_mi_state(maps:update(mi, NewMi, State)));
  % The heart of the recursive algorithm
    {sendy, Y} ->
      receive_loop(maybe_update_mi(Y, State));
  % TODO Wahlnachricht für die Terminierung der aktuellen Berechnung;
  % TODO Initiator ist der Initiator dieser Wahl (Name des ggT-Prozesses, keine PID!) und From (ist PID) ist sein Absender.
    {_From, {vote, _Initiator}} -> ok
% Another ggT Process votes for a termination
%%    {voteYes, Name} ->
  %werkzeug:logging(lists:concat([GGTName, "@vsp"]), lists:concat(["Received vote from ", Name, "\n"])),
  %if
  %V >= Quota -> Koordinator ! kill;
  %true -> receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V + 1, L, R, Mi, LastReceive)
  %end;
% Just a getter for current Mi Value
%%    {From, tellmi} -> From ! {mi, Mi};
%%
%%    {From, pingGGT} -> From ! {pongGGT, GGTName};
%%    kill -> exit(self(), normal), ok
  end,
  receive_loop(State).


%% Starts the ggT Process and registers at the coordinator, nameservice and locally at the node
start(WorkingTime, TerminationTime, Quota, GgTName, Coordinator, NameService) ->
  State = #{
    ggtname => GgTName,
    workingtime => WorkingTime,
    termtime => TerminationTime,
    quota => Quota,
    coordinator => Coordinator,
    nameservice => NameService,
    leftneighbor => undefined,
    rightneigbor => undefined,
    mi => undefined,
    yesVotes => 0,
    lastMiUpdate => 0,
    isTerminating => false
  },
  spawn(?MODULE, start_ggt_process, [State]).
