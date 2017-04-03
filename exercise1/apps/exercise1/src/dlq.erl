-module(dlq).
-include_lib("eunit/include/eunit.hrl").
%% API
-export([initDLQ/2, delDLQ/1, expectedNr/1, push2DLQ/3, deliverMSG/4]).

% initialisiert die DLQ mit Kapazität Size. Bei Erfolg wird eine leere DLQ zurück geliefert.
% Datei kann für ein logging genutzt werden.
initDLQ(Size, Datei) ->
  werkzeug:logging(Datei, lists:concat(["DLQ>>> initialized with capacity: ", Size, ".\n"])),
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

deliverMSG(_MSGNr, _ClientPID, _Queue, _Datei) -> ok.