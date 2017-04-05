-module(hbq).
-export([initHBQ/2]).


%Size ist die Größe der DLQ
%Wenn die HBQ mehr elemente als 2/3 von Size enthält, wird eine nachricht "aufgefüllt"
initHBQ(Size, Datei) ->
  werkzeug:logging(Datei, lists:concat(["HBQ>>> initialized with capacity: ", Size, "\n"])),
  loop([], Size, 0, dlq:initDLQ(100, "logging")).


loop(Messages, HBQSize, CurrentNNr, DLQ) ->
  receive
    {deliverMSG,NNr,ToClient, Datei} ->
      dlq:deliverMSG(NNr, ToClient, [Messages, HBQSize], Datei);
    {dellHBQ} ->
      exit("dellHBQ was called"), ok;
    {pushHBQ, {[NNr, Msg, TSclientout, TShbqin], Datei}}  ->
      werkzeug:logging(Datei, lists:concat(["HBQ>>> pushing message: ", NNr, "\n"])),
      Messages = lists:append(Messages, [[NNr, Msg, TSclientout, TShbqin]]),
      Messages = lists:keysort(1, Messages),
      Size = length(Messages),
      if NNr == CurrentNNr ->
        {CurrentNNr, Messages} = pushAllConsecutiveSequenceNumbers(Messages, DLQ, Datei),
        loop(Messages, HBQSize, CurrentNNr + 1, DLQ);
        NNr >= CurrentNNr ->
          %We are still waiting for older messages. Just append the message to the holdback queue
          loop(lists:append([NNr, Msg, TSclientout, TShbqin], Messages), HBQSize, CurrentNNr, DLQ);
        Size >= HBQSize * (2 / 3) ->
          % TODO: The HBQ is full, insert a placeholder message for the last gap and send all older messages than CurrentNNr
        ok
    end
  end
.


%deliverMSG(MSGNr, ClientPID, Queue, Datei)
pushAllConsecutiveSequenceNumbers([[NNr, _, _, _] | Tail], DLQ, Datei) ->
  dlq:push2DLQ(NNr, DLQ, Datei),
  [NNr2, _, _, _] = Tail,
  if NNr == NNr2  - 1 ->
      pushAllConsecutiveSequenceNumbers(Tail, DLQ, Datei)
  end,
  werkzeug:logging(Datei, lists:concat(["HBQ>>>sent all messages to dlq until NNR: ", NNr, "\n"])),
  {NNr, Tail}

.



