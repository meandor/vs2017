-module(dlq).

%% API
-export([initDLQ/2, delDLQ/1, expectedNr/1, push2DLQ/3, deliverMSG/4]).

% initialisiert die DLQ mit Kapazität Size. Bei Erfolg wird eine leere DLQ zurück geliefert.
% Datei kann für ein logging genutzt werden.
initDLQ(Size,Datei) ->
    {[],Size,Datei}.

delDLQ(Queue)

expectedNr(Queue)

push2DLQ([NNr,Msg,TSclientout,TShbqin],Queue,Datei)

deliverMSG(MSGNr,ClientPID,Queue,Datei)
