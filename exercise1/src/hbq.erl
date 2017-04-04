-module(hbq).
-export([initHBQ/2]).


%Size ist die Größe der DLQ
%Wenn die HBQ mehr elemente als 2/3 von Size enthält, wird eine nachricht "aufgefüllt"
initHBQ(Size, Datei) ->
  werkzeug:logging(Datei, lists:concat(["HBQ>>> initialized with capacity: ", Size, "\n"])),
  [[], Size], loop([], Size, 0).


loop(Messages, HBQSize, CurrentNNr) ->
  receive
    {deliverMSG,NNr,ToClient, Datei} ->
      dlq:deliverMSG(NNr, ToClient, [[], _Size], Datei);
    {dellHBQ} ->
      exit("dellHBQ was called"), ok;
    {pushHBQ, {[NNr, Msg, TSclientout, TShbqin], [DLQ, Size], Datei}}  ->
      lists:append([NNr, Msg, TSclientout, TShbqin], Messages),
      Size = length(Messages),
      if NNr == CurrentNNr ->
        %TODO Push all messages older than NNr, if consecutive sequence numbers occur
        dlq:push2DLQ([NNr, Msg, TSclientout, TShbqin], [DLQ, Size], Datei),
        loop(Messages, HBQSize, CurrentNNr + 1);
        NNr >= CurrentNNr ->
          %We are still waiting for older messages. Just append the message to the holdback queue
          loop(lists:append([NNr, Msg, TSclientout, TShbqin], Messages), HBQSize, CurrentNNr);
        Size >= HBQSize * (2 / 3) ->
          % TODO: The HBQ is full, insert a placeholder message for the last gap and send all older messages than CurrentNNr
        ok
    end
  end
.

