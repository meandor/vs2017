%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt).
-export([start/6, start_ggt_process/1, reset_terminate_timer/1, maybe_update_mi/2, update_neighbours/3, set_pm/2, term_request/1]).

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
  TermTime = maps:get(termtime, State) * 1000,
  NewState = maps:update(terminateTimer, timer:apply_after(TermTime, ?MODULE, term_request, [State]), State),
  handle_messages(NewState).

reset_terminate_timer(State) ->
  timer:cancel(maps:get(terminateTimer, State)),
  TermTime = maps:get(termtime, State) * 1000,
  NewTimer = timer:apply_after(TermTime, ?MODULE, term_request, [State]),
  maps:update(terminateTimer, NewTimer, State).

update_neighbours(Left, Right, State) ->
  log(State, ["left neighbour registered: ", atom_to_list(Left)]),
  log(State, ["right neighbour registered: ", atom_to_list(Right)]),
  maps:update(rightneigbor, Right, maps:update(leftneighbor, Left, State)).

set_pm(Mi, State) ->
  log(State, ["setpm: ", integer_to_list(Mi)]),
  NewState = maps:update(mi, Mi, maps:update(isTerminating, false, State)),
  reset_terminate_timer(NewState).

maybe_update_mi(Y, State) ->
  Mi = maps:get(mi, State),
  L = maps:get(leftneighbor, State),
  R = maps:get(rightneigbor, State),
  Coordinator = maps:get(coordinator, State),
  GgTName = maps:get(ggtname, State),
  if
    Y < Mi ->
      timer:sleep(maps:get(workingtime, State) * 1000),
      NewMi = ((Mi - 1) rem Y) + 1,
      L ! {sendy, NewMi},
      R ! {sendy, NewMi},
      Coordinator ! {briefmi, {GgTName, NewMi, erlang:now()}},
      log(State, ["sendy: ", integer_to_list(Y), " (", integer_to_list(Mi), "); new mi: ", integer_to_list(NewMi), " ", werkzeug:timeMilliSecond()]),
      NewState = maps:update(mi, NewMi, maps:update(isTerminating, false, State)),
      reset_terminate_timer(NewState);
    true ->
      log(State, ["sendy: ", integer_to_list(Y), " (", integer_to_list(Mi), "); no new mi"]),
      maps:update(isTerminating, false, State)
  end.

term_request(State) ->
  IsTerminating = maps:get(isTerminating, State),
  if
    IsTerminating ->
      ok;
    true ->
      maps:get(nameservice, State) ! {self(), {multicast, vote, maps:get(ggtname, State)}}
  end.

handle_messages(State) ->
  receive
  % Sets the right and left neighbour processes for the recursive algorithm
    {setneighbors, LeftNeighbour, RightNeighbour} ->
      handle_messages(update_neighbours(LeftNeighbour, RightNeighbour, State));
  % Sets a new Mi to calculate
    {setpm, NewMi} ->
      handle_messages(set_pm(NewMi, State));
  % Starts the algorithm to calculate a ggT if possible
    {sendy, Y} ->
      handle_messages(maybe_update_mi(Y, State));
    {_From, {vote, _Initiator}} -> ok;
    {From, tellmi} ->
      From ! {mi, maps:get(mi, State)};
    {From, pingGGT} ->
      From ! {pongGGT, maps:get(ggtname, State)};
    kill -> exit(self(), normal), ok
  end,
  handle_messages(State).


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
    terminateTimer => undefined,
    isTerminating => false
  },
  spawn(?MODULE, start_ggt_process, [State]).
