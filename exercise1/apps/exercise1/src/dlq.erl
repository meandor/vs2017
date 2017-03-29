-module(dlq).
-include_lib("eunit/include/eunit.hrl").
%% API
-export([initDLQ/2, delDLQ/1, expectedNr/1, push2DLQ/3, deliverMSG/4]).

% initialisiert die DLQ mit Kapazität Size. Bei Erfolg wird eine leere DLQ zurück geliefert.
% Datei kann für ein logging genutzt werden.
initDLQ(Size,Datei) ->
    {[],Size,Datei}.

delDLQ(Queue) -> ok.

expectedNr(Queue) -> ok.

push2DLQ([NNr,Msg,TSclientout,TShbqin],Queue,Datei) -> ok.

deliverMSG(MSGNr,ClientPID,Queue,Datei) -> ok.