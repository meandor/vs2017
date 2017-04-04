-module(dlq).
-export([initDLQ/2, delDLQ/1, expectedNr/1, push2DLQ/3, deliverMSG/4]).

% initialisiert die DLQ mit Kapazität Size. Bei Erfolg wird eine leere DLQ zurück geliefert.
% Datei kann für ein logging genutzt werden.
initDLQ(Size, Datei) ->
  werkzeug:logging(Datei, lists:concat(["DLQ>>> initialized with capacity: ", Size, "\n"])),
  [[], Size].

% Löschen der DLQ
delDLQ(_) -> ok.

% liefert die Nachrichtennummer, die als nächstes in der DLQ gespeichert werden kann. Bei leerer DLQ ist dies 1.
expectedNr([[], _Size]) -> 1;
expectedNr([[[NNr, _Msg, _TSclientout, _TShbqin, _TSdlqin] | _Rest], _Size]) -> NNr + 1.

% speichert die Nachricht [NNr,Msg,TSclientout,TShbqin] in der DLQ Queue und fügt ihr einen Eingangszeitstempel an
% (einmal an die Nachricht Msg und als expliziten Zeitstempel TSdlqin mit erlang:now() an die Liste an. Bei Erfolg wird
% die modifizierte DLQ zurück geliefert. Datei kann für ein logging genutzt werden.
push2DLQ([NNr, Msg, TSclientout, TShbqin], [DLQ, Size], Datei) ->
  werkzeug:logging(Datei, lists:concat(["DLQ>>> message number ", NNr, " added to DLQ\n"])),
  if
    length(DLQ) < Size ->
      [[[NNr, Msg, TSclientout, TShbqin, erlang:now()] | DLQ], Size];
    length(DLQ) == Size ->
      [[[NNr, Msg, TSclientout, TShbqin, erlang:now()] | lists:droplast(DLQ)], Size]
  end.

findNextBiggerNNrMessage(NNr, Queue) -> ok.

% Ausliefern einer Nachricht an einen Leser-Client
deliverMSG(MSGNr, ClientPID, Queue, Datei) -> deliverMSG(MSGNr, ClientPID, Queue, Datei, Queue).

% Sollte die Nachrichtennummer nicht mehr vorhanden sein, wird die nächst größere in der DLQ vorhandene Nachricht gesendet
deliverMSG(MSGNr, ClientPID, Queue, Datei, [[], _Size]) ->
  [NNr, Msg, TSclientout, TShbqin, TSdlqin] = findNextBiggerNNrMessage(MSGNr, Queue),
  ClientPID ! {send, [NNr, Msg, TSclientout, TShbqin, TSdlqin, erlang:now()]},
  werkzeug:logging(Datei, lists:concat(["DLQ>>> message ", NNr, " to Client<", ClientPID, "> sent\n"])),
  NNr;

% Falls Nachricht mit MSGNr vorhanden ist, senden an Client, ansonsten weitersuchen im DLQRest
deliverMSG(MSGNr, ClientPID, Queue, Datei, [[[NNr, Msg, TSclientout, TShbqin, TSdlqin] | DLQRest], Size]) ->
  if
    NNr == MSGNr ->
      ClientPID ! {send, [NNr, Msg, TSclientout, TShbqin, TSdlqin, erlang:now()]},
      werkzeug:logging(Datei, lists:concat(["DLQ>>> message ", MSGNr, " to Client<", ClientPID, "> sent\n"])),
      MSGNr;
    MSGNr /= NNr ->
      deliverMSG(MSGNr, ClientPID, Queue, Datei, [DLQRest, Size])
  end.