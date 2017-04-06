-module(reader).

-export([start_reading/4]).

mark_my_reader_message(NNr, Msg, ReaderNNrs) ->
  ReaderMessage = lists:member(NNr, ReaderNNrs),
  if
    ReaderMessage -> Msg ++ "*******";
    true -> Msg
  end.

write_message([NNr, Msg, _TSclientout, _TShbqin, TSdlqin, TSdlqout], Logfile, ReaderNNrs) ->
  Now = erlang:timestamp(),
  ValidTSdlqin = werkzeug:validTS(TSdlqin),
  ValidTSdlqout = werkzeug:validTS(TSdlqout),
  TSdlqinFromFuture = werkzeug:lessTS(Now, TSdlqin),
  TSdlqoutFromFuture = werkzeug:lessTS(Now, TSdlqout),
  MarkedMessage = mark_my_reader_message(NNr, Msg, ReaderNNrs),
  if
    ValidTSdlqin and TSdlqinFromFuture ->
      Diff = werkzeug:now2stringD(werkzeug:diffTS(TSdlqin, Now)),
      NewMsg = MarkedMessage ++ " time difference: " ++ Diff,
      werkzeug:logging(Logfile, NewMsg);
    ValidTSdlqout and TSdlqoutFromFuture ->
      Diff = werkzeug:now2stringD(werkzeug:diffTS(TSdlqout, Now)),
      NewMsg = MarkedMessage ++ " time difference: " ++ Diff,
      werkzeug:logging(Logfile, NewMsg);
    true ->
      werkzeug:logging(Logfile, MarkedMessage)
  end.

start_reading(true, _Logfile, _ReaderNNrs, _ServerPID) -> ok;

start_reading(false, Logfile, ReaderNNrs, ServerPID) ->
  ServerPID ! {self(), getmessages},
  receive
    {reply, Message, Terminated} ->
      write_message(Message, Logfile, ReaderNNrs),
      start_reading(Terminated, Logfile, ReaderNNrs, ServerPID)
  end.
