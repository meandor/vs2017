-module(dlq).
-include_lib("eunit/include/eunit.hrl").
%% API
-export([initDLQ/2, delDLQ/1, expectedNr/1, push2DLQ/3, deliverMSG/4]).

% initialisiert die DLQ mit Kapazität Size. Bei Erfolg wird eine leere DLQ zurück geliefert.
% Datei kann für ein logging genutzt werden.
initDLQ(Size,Datei) ->
    {[],Size,Datei}.

delDLQ(Queue) -> ok.

% liefert die Nachrichtennummer, die als nächstes in der DLQ gespeichert werden kann. Bei leerer DLQ ist dies 1.
expectedNr({[],_,_}) -> 1;
expectedNr({[{NNr, _} | _],_,_}) ->  NNr + 1.

push2DLQ([NNr,Msg,TSclientout,TShbqin],Queue,Datei) -> ok.

deliverMSG(MSGNr,ClientPID,Queue,Datei) -> ok.