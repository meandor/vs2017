%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ggt).
-export([start/6, receive_loop/6]).


loggingAtom(GGTName) ->
  LogfileName = lists:concat(["GGT@", GGTName, ".log"]),
  erlang:list_to_atom(LogfileName).

start(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, Nameservice) ->
  Koordinator ! {hello, GGTName},
  % Er registriert sich ebenfalls lokal auf der Erlang-Node mit seinem Namen (register).
  % Der ggT-Prozess erwartet dann vom Koordinator die Informationen über seine Nachbarn (setneighbors).
  spawn(?MODULE, receive_loop, [WorkingTime, TerminationTime, Quota, GGTName, Nameservice, Koordinator]).

receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Nameservice, Koordinator) ->
  Nameservice ! {self(), {rebind, GGTName, node()}},
  werkzeug:register_safe(GGTName, self()),
  receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, 0, undefined, undefined, -1).

receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, L, R, Mi) ->
  receive

  % Sets the right and left neighbour processes for the recursive algorithm
    {setneighbors, LeftNeighbour, RightNeighbour} ->
      receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, LeftNeighbour, RightNeighbour, Mi);

  % Sets a new Mi to calculate
    {setpm, MiNeu} -> receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, L, R, MiNeu);

  % The heart of the recursive algorithm
    {sendy, Y} -> if
                    Y < Mi ->
                      NewMi = ((Mi - 1) rem Y) + 1,
                      L ! {sendy, NewMi},
                      R ! {sendy, NewMi},
                      receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, L, R, NewMi)
                  end;

  % TODO Wahlnachricht für die Terminierung der aktuellen Berechnung;
  % TODO Initiator ist der Initiator dieser Wahl (Name des ggT-Prozesses, keine PID!) und From (ist PID) ist sein Absender.
    {From, {vote, Initiator}} -> ok;

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
    kill -> werkzeug:logging(lists:concat([GGTName, "@vsp"]), "Kill received"), exit(self(), normal), ok
  end,
  receive_loop(WorkingTime, TerminationTime, Quota, GGTName, Koordinator, V, L, R, Mi)
.

