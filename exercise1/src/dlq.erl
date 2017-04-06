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
  if
    length(DLQ) < Size ->
      werkzeug:logging(Datei, lists:concat(["DLQ>>> message number ", NNr, " added to DLQ\n"])),
      [[[NNr, Msg, TSclientout, TShbqin, erlang:now()] | DLQ], Size];
    length(DLQ) =:= Size ->
      [LastNNr, _Msg, _TSclientout, _TShbqin, _TSdlqin] = lists:last(DLQ),
      werkzeug:logging(Datei, lists:concat(["DLQ>>> message number ", LastNNr, " dropped from DLQ\n"])),
      werkzeug:logging(Datei, lists:concat(["DLQ>>> message number ", NNr, " added to DLQ\n"])),
      [[[NNr, Msg, TSclientout, TShbqin, erlang:now()] | lists:droplast(DLQ)], Size]
  end.

% Sendet eine Nachricht an ClientPID
sendMessage(ClientPID, Message, Terminated) ->
  ClientPID ! {reply, Message, Terminated}.

% Nachricht wurde nicht gefunden -> nnr ist zu hoch oder DLQ leer, verschicke dummy Nachricht
findMessageToDeliver(_MSGNr, []) ->
  [-1, "No new messages", -1, -1, -1];
% Nachricht oder nächst höhere Nachricht wurde gefunden und wird returned
findMessageToDeliver(MSGNr, [[NNr, Msg, TSclientout, TShbqin, TSdlqin] | _DLQRest]) when NNr >= MSGNr ->
  [NNr, Msg, TSclientout, TShbqin, TSdlqin];
% Rekursionsschritt, suche den rest der liste ab
findMessageToDeliver(MSGNr, [[NNr, _Msg, _TSclientout, _TShbqin, _TSdlqin] | DLQRest]) when NNr < MSGNr ->
  findMessageToDeliver(MSGNr, DLQRest).

% Ausliefern einer Nachricht an einen Leser-Client. Die neuste Nachricht ist links.
% Es wird die Nachricht ausgeliefert, die von hinten als erstes größer gleich der MSGNr ist.
% Dadurch wird entweder genau die MSGNr ausgeliefert oder die nächst höchste, da die DLQ sortiert ist.
deliverMSG(MSGNr, ClientPID, [Queue, Size], Datei) ->
  [NNr, Msg, TSclientout, TShbqin, TSdlqin] = findMessageToDeliver(MSGNr, lists:reverse(Queue)),
  Terminated = (expectedNr([Queue, Size]) - 1 =:= NNr) or (NNr =:= -1),
  sendMessage(ClientPID, [NNr, Msg, TSclientout, TShbqin, TSdlqin, erlang:now()], Terminated),
  werkzeug:logging(Datei, lists:concat(["DLQ>>> message ", NNr, " sent to Client ", pid_to_list(ClientPID), "\n"])),
  NNr.
