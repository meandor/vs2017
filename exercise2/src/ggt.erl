%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt).
-export([start/6, start_ggt_process/1]).

-spec log(map(), list()) -> atom().
log(Config, Message) ->
  GgTName = maps:get(ggtname, Config),
  Logfile = list_to_atom(lists:concat(["GGTP_", atom_to_list(GgTName), "@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  werkzeug:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

start_ggt_process(Config) ->
  GgTName = maps:get(ggtname, Config),
  Coordinator = maps:get(coordinator, Config),
  WorkingTime = maps:get(workingtime, Config),
  TermTime = maps:get(termtime, Config),
  Quota = maps:get(quota, Config),
  log(Config, [atom_to_list(GgTName), " starttime: ", werkzeug:timeMilliSecond(), " with PID ", pid_to_list(self()), " on ", atom_to_list(node())]),
  register(GgTName, self()),
  log(Config, ["registered locally"]),
  maps:get(nameservice, Config) ! {self(), {rebind, GgTName, node()}},
  receive
    ok -> log(Config, ["registered at nameservice"])
  end,
  Coordinator ! {hello, GgTName},
  log(Config, ["registered at coordinator"]),
  receive_loop(WorkingTime, TermTime, Quota, GgTName, Coordinator, 0, undefined, undefined, -1, erlang:now()).

receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, L, R, Mi, LastReceive) ->
  receive

  % Sets the right and left neighbour processes for the recursive algorithm
    {setneighbors, LeftNeighbour, RightNeighbour} ->
      receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, LeftNeighbour, RightNeighbour, Mi, erlang:now());

  % Sets a new Mi to calculate
    {setpm, MiNeu} -> receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, L, R, MiNeu, erlang:now());

  % The heart of the recursive algorithm
    {sendy, Y} ->
      if Y < Mi ->
        timer:sleep(WorkingTime),
        NewMi = ((Mi - 1) rem Y) + 1,
        werkzeug:logging(lists:concat([GGTName, "@vsp"]), lists:concat(["mi: ", Y, ", Old mi:", Mi, " NEW MI:", NewMi, "\n"])),
        L ! {sendy, NewMi},
        R ! {sendy, NewMi},
        Koordinator ! {briefmi, {GGTName, NewMi, erlang:now()}},
        receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, L, R, NewMi, erlang:now())
      ;true -> werkzeug:logging(lists:concat([GGTName, "@vsp"]), lists:concat(["else zweig \n"]))
      end;

  % TODO Wahlnachricht für die Terminierung der aktuellen Berechnung;
  % TODO Initiator ist der Initiator dieser Wahl (Name des ggT-Prozesses, keine PID!) und From (ist PID) ist sein Absender.
    {_From, {vote, _Initiator}} -> ok;

  % Another ggT Process votes for a termination
    {voteYes, Name} ->
      werkzeug:logging(lists:concat([GGTName, "@vsp"]), lists:concat(["Received vote from ", Name, "\n"])),
      if
        V >= Quota -> Koordinator ! kill;
        true -> receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V + 1, L, R, Mi)
      end;
  % Just a getter for current Mi Value
    {From, tellmi} -> From ! {mi, Mi};

    {From, pingGGT} -> From ! {pongGGT, GGTName};
    kill -> exit(self(), normal), ok
  end,
  receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, L, R, Mi)
.

%% Starts the ggT Process and registers at the coordinator, nameservice and locally at the node
start(WorkingTime, TerminationTime, Quota, GgTName, Coordinator, NameService) ->
  Config = #{
    ggtname => GgTName,
    workingtime => WorkingTime,
    termtime => TerminationTime,
    quota => Quota,
    coordinator => Coordinator,
    nameservice => NameService,
    leftneighbor => undefined,
    rightneigbor => undefined,
    mi => -1,
    v => 0
  },
  spawn(?MODULE, start_ggt_process, [Config]).
