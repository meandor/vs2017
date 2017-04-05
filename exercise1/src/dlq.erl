-module(dlq).
-export([initDLQ/2, delDLQ/1, expectedNr/1, push2DLQ/3, deliverMSG/4]).

maxInt() -> 134217728.

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

% Hilfsfunktion um im fold die Nachricht mit der kleinsten nächst höchsten NNr in der DLQ zu finden
maxNextMessage(MSGNr, NewMessage, CurrentMessage) ->
  [CurrentNNr, _CurrentMsg, _CurrentTSclientout, _CurrentTShbqin, _CurrentTSdlqin] = CurrentMessage,
  [NewNNr, _NewMsg, _NewTSclientout, _NewTShbqin, _NewTSdlqin] = NewMessage,
  if NewNNr < CurrentNNr, NewNNr > MSGNr ->
    NewMessage;
    true -> CurrentMessage
  end.

% Liefert die nächst höhere Nachricht im Bezug auf NNr
findNextBiggerNNrMessage(NNr, [Queue, _Size]) ->
  Start = [maxInt(), "", 1, 1, 1],
  lists:foldl(fun(Elem, Acc) -> maxNextMessage(NNr, Elem, Acc) end, Start, Queue).

% Returned boolean falls noch mehr Nachrichten in der DLQ vorhanden sind
remainingMessagesExist(MsgNr, [[[NNr, _Msg, _TSclientout, _TShbqin, _TSdlqin] | _DLQRest], _Size]) ->
  MsgNr < NNr.

% Sendet eine Nachricht an ClientPID
sendMessage(ClientPID, Message, Terminated) ->
  ClientPID ! {reply, Message, Terminated}.

% TODO: Ein Leser-Client bekommt auf Anfrage gemäß Nachrichtennummerierung eine noch nicht an ihn ausgelieferte und beim
% TODO: Server bekannte Textzeile geliefert. In einem Flag wird ihm mitgeteilt, ob es noch weitere, für ihn unbekante
% TODO: Nachrichten gibt. Zudem wird ihm explizit die Nummer dieser Nachricht übermittelt. Wenn der Leser-Client nach
% TODO: neuen Nachrichten beim Server anfragt, dort jedoch keine neuen bzw. überhaupt noch keine Nachrichten vorhanden
% TODO: sind, sendet der Server eine nicht leere dummy-Nachricht.
% Ausliefern einer Nachricht an einen Leser-Client
deliverMSG(MSGNr, ClientPID, Queue, Datei) -> deliverMSG(MSGNr, ClientPID, Queue, Datei, Queue).

% Sollte die Nachrichtennummer nicht mehr vorhanden sein, wird die nächst größere in der DLQ vorhandene Nachricht gesendet
deliverMSG(MSGNr, ClientPID, Queue, Datei, [[], _Size]) ->
  [NNr, Msg, TSclientout, TShbqin, TSdlqin] = findNextBiggerNNrMessage(MSGNr, Queue),
  Terminated = remainingMessagesExist(NNr, Queue),
  sendMessage(ClientPID, [NNr, Msg, TSclientout, TShbqin, TSdlqin, erlang:now()], Terminated),
  werkzeug:logging(Datei, lists:concat(["DLQ>>> message ", NNr, " to Client<", ClientPID, "> sent\n"])),
  NNr;

% Falls Nachricht mit MSGNr vorhanden ist, senden an Client, ansonsten weitersuchen im DLQRest
deliverMSG(MSGNr, ClientPID, Queue, Datei, [[[NNr, Msg, TSclientout, TShbqin, TSdlqin] | DLQRest], Size]) ->
  if
    NNr =:= MSGNr ->
      Terminated = remainingMessagesExist(NNr, Queue),
      sendMessage(ClientPID, [NNr, Msg, TSclientout, TShbqin, TSdlqin, erlang:now()], Terminated),
      werkzeug:logging(Datei, lists:concat(["DLQ>>> message ", MSGNr, " to Client<", ClientPID, "> sent\n"])),
      MSGNr;
    true ->
      deliverMSG(MSGNr, ClientPID, Queue, Datei, [DLQRest, Size])
  end.