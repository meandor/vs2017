%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt).
-export([start/6, receive_loop/7]).

-spec log(map(), list()) -> atom().
log(Config, Message) ->
  GgTName = maps:get(ggtname, Config),
  Logfile = list_to_atom(lists:concat(["GGTP_", atom_to_list(GgTName), "@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  werkzeug:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Nameservice, Koordinator, Config) ->
  log(Config, [atom_to_list(GGTName), " starttime: ", werkzeug:timeMilliSecond(), " with PID ", pid_to_list(self()), " on ", atom_to_list(node())]),
  register(GGTName, self()),
  log(Config, ["registered locally"]),
  Nameservice ! {self(), {rebind, GGTName, node()}},
  receive
    ok -> log(Config, ["registered at nameservice"])
  end,
  Koordinator ! {hello, GGTName},
  log(Config, ["registered at coordinator"]),
  receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, 0, undefined, undefined, -1).

receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, L, R, Mi) ->
  receive

  % Sets the right and left neighbour processes for the recursive algorithm
    {setneighbors, LeftNeighbour, RightNeighbour} ->
      receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, LeftNeighbour, RightNeighbour, Mi);

  % Sets a new Mi to calculate
    {setpm, MiNeu} -> receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, L, R, MiNeu);

  % The heart of the recursive algorithm
    {sendy, Y} ->

      if Y < Mi ->
        NewMi = ((Mi - 1) rem Y) + 1,
        werkzeug:logging(lists:concat([GGTName, "@vsp"]), lists:concat(["mi: ", Y, ", Old mi:", Mi, " NEW MI:", NewMi, "\n"])),
        L ! {sendy, NewMi},
        R ! {sendy, NewMi},
        Koordinator ! {briefmi, {GGTName, NewMi, erlang:now()}},
        receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, L, R, NewMi)
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
  Config = #{ggtname => GgTName},
  spawn(?MODULE, receive_loop, [WorkingTime, TerminationTime, Quota, GgTName, NameService, Coordinator, Config]).
