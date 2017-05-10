%%%-------------------------------------------------------------------
%%% @doc
%%% Handles all ggT related calculations of the algorithm.
%%% For more infos have a look at the documentation section 3.2
%%% @end
%%%-------------------------------------------------------------------
-module(ggt).
-export([start/6, start_ggt_process/1, reset_terminate_timer/1, maybe_update_mi/2, update_neighbours/3, set_pm/2, term_request/1, voting_response/2, maybe_send_brief_term/2]).

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
  {ok, TRef} = timer:apply_after(TermTime, ?MODULE, term_request, [State]),
  NewState = maps:update(terminateTimer, TRef, State),
  handle_messages(NewState).

reset_terminate_timer(State) ->
  timer:cancel(maps:get(terminateTimer, State)),
  TermTime = maps:get(termtime, State) * 1000,
  {ok, TRef} = timer:apply_after(TermTime, ?MODULE, term_request, [State]),
  UpdatedTerm = maps:update(isTerminating, false, State),
  UpdatedTimestamp = maps:update(lastNumberReceived, erlang:timestamp(), UpdatedTerm),
  maps:update(terminateTimer, TRef, UpdatedTimestamp).


bind_ggt(GgTName, State) ->
  maps:get(nameservice, State) ! {self(), {lookup, GgTName}},
  receive
    not_found ->
      log(State, ["Warning: Could not find the ggT process '", atom_to_list(GgTName), "'"]);
    {pin, GgTPID} ->
      GgTPID
  end.

update_neighbours(Left, Right, State) ->
  log(State, ["left neighbour registered: ", atom_to_list(Left)]),
  LeftPID = bind_ggt(Left, State),
  log(State, ["left neighbour bound"]),
  log(State, ["right neighbour registered: ", atom_to_list(Right)]),
  RightPID = bind_ggt(Right, State),
  log(State, ["right neighbour bound"]),
  UpdatedNeighbourNamesState = maps:update(rightneigbor, Right, maps:update(leftneighbor, Left, State)),
  maps:update(leftneighborPID, LeftPID, maps:update(rightneighborPID, RightPID, UpdatedNeighbourNamesState)).

set_pm(Mi, State) ->
  log(State, ["setpm: ", integer_to_list(Mi)]),
  NewState = reset_terminate_timer(State),
  maps:update(mi, Mi, NewState).

maybe_update_mi(Y, State) ->
  Mi = maps:get(mi, State),
  L = maps:get(leftneighborPID, State),
  R = maps:get(rightneighborPID, State),
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
      maps:update(mi, NewMi, reset_terminate_timer(State));
    true ->
      log(State, ["sendy: ", integer_to_list(Y), " (", integer_to_list(Mi), "); no new mi"]),
      reset_terminate_timer(State)
  end.

term_request(State) ->
  IsTerminating = maps:get(isTerminating, State),
  if
    IsTerminating ->
      ok;
    true ->
      log(State, ["Start termination voting ", werkzeug:timeMilliSecond()]),
      maps:get(nameservice, State) ! {self(), {multicast, vote, maps:get(ggtname, State)}}
  end.

-spec voting_response(atom(), map()) -> atom().
voting_response(GgTName, State) ->
  Threshold = round(maps:get(termtime, State) / 2),
  log(State, ["DEBUG: voting response | threshold: ", integer_to_list(Threshold)]),
  LastNumberReceived = maps:get(lastNumberReceived, State),
  if
    LastNumberReceived =:= 0 ->
      log(State, ["Voting no for term request with ignoring"]),
      ok;
    true ->
      PassedTime = round(timer:now_diff(erlang:timestamp(), maps:get(lastNumberReceived, State)) / 1000000),
      log(State, ["DEBUG: voting response | passed time: ", integer_to_list(PassedTime)]),
      if
        PassedTime > Threshold ->
          maps:get(nameservice, State) ! {self(), {lookup, GgTName}},
          receive
            not_found ->
              log(State, ["Warning: Could not find the ggT process '", atom_to_list(GgTName), "'"]);
            {pin, Initiator} ->
              log(State, ["Sending voteYes to ", atom_to_list(GgTName)]),
              Initiator ! {voteYes, maps:get(ggtname, State)}
          end;
        true ->
          log(State, ["Voting no for term request with ignoring"]),
          ok
      end
  end.

maybe_send_brief_term(GgTName, State) ->
  CurrentVotes = maps:get(yesVotes, State),
  Quota = maps:get(quota, State),
  NewVotes = CurrentVotes + 1,
  log(State, ["received yes vote from ", atom_to_list(GgTName), " with a total votes of ", integer_to_list(NewVotes), " ", werkzeug:timeMilliSecond()]),
  NewState = maps:update(yesVotes, NewVotes, State),
  if
    NewVotes == Quota ->
      Coordinator = maps:get(coordinator, State),
      Coordinator ! {self(), briefterm, {maps:get(ggtname, State), maps:get(mi, State), erlang:now()}},
      NewTermsCount = maps:get(terminatedCalculations, NewState) + 1,
      log(State, ["Send #", integer_to_list(NewTermsCount), " terminated brief to coordinator"]),
      maps:update(terminatedCalculations, NewTermsCount, maps:update(yesVotes, 0, NewState));
    true ->
      NewState
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
  % Starts the voting process answer
    {_From, {vote, Initiator}} ->
      voting_response(Initiator, State),
      handle_messages(State);
  % Sends brief mi to coordinator if enough yes votes came in
    {voteYes, Name} ->
      handle_messages(maybe_send_brief_term(Name, State));
  % Used for getting status
    {From, tellmi} ->
      From ! {mi, maps:get(mi, State)},
      handle_messages(State);
  % Used for getting status
    {From, pingGGT} ->
      From ! {pongGGT, maps:get(ggtname, State)},
      handle_messages(State);
    kill ->
      log(State, ["shutting down ggt"]),
      exit(self(), normal),
      ok
  end.


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
    leftneighborPID => undefined,
    rightneighborPID => undefined,
    mi => undefined,
    yesVotes => 0,
    terminateTimer => undefined,
    lastNumberReceived => 0,
    isTerminating => false,
    terminatedCalculations => 0
  },
  spawn(?MODULE, start_ggt_process, [State]).
