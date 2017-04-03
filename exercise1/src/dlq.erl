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
expectedNr([[], _]) -> 1;
expectedNr([[[NNr, _, _, _] | _], _]) -> NNr + 1.

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

findNextBiggerNNr(NNr, Queue) -> ok.

% Ausliefern einer Nachricht an einen Leser-Client
deliverMSG(MSGNr, _ClientPID, [[], _Size], Datei) ->
  % TODO: Sollte die Nachrichtennummer nicht mehr vorhanden sein, wird die nächst größere in der DLQ vorhandene Nachricht gesendet
  werkzeug:logging(Datei, lists:concat(["DLQ>>> message ", MSGNr, " could not be sent because it was not found\n"])),
  fail;
deliverMSG(MSGNr, ClientPID, [[[NNr, _, _, _, _] | DLQRest], Size], Datei) ->
  if
    NNr == MSGNr ->
      werkzeug:logging(Datei, lists:concat(["DLQ>>> message ", MSGNr, " to Client<", ClientPID, "> sent\n"])),
      % TODO: sendet die Nachricht MSGNr an den Leser-Client ClientPID. Dabei wird ein Ausgangszeitstempel TSdlqout mit erlang:now() an das Ende der Nachrichtenliste angefügt.
      MSGNr;
    MSGNr /= NNr ->
      deliverMSG(MSGNr, ClientPID, [DLQRest, Size], Datei)
  end.