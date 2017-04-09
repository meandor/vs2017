-module(reader).

-export([start_reading/4]).

formatMessage(NNr, Msg, ReaderNNrs) ->
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
  FormattedMessage = formatMessage(NNr, Msg, ReaderNNrs),
  if
    ValidTSdlqin and TSdlqinFromFuture ->
      Diff = werkzeug:now2stringD(werkzeug:diffTS(TSdlqin, Now)),
      NewMsg = FormattedMessage ++ " time difference: " ++ Diff,
      werkzeug:logging(Logfile, NewMsg ++ "\n");
    ValidTSdlqout and TSdlqoutFromFuture ->
      Diff = werkzeug:now2stringD(werkzeug:diffTS(TSdlqout, Now)),
      NewMsg = FormattedMessage ++ " time difference: " ++ Diff,
      werkzeug:logging(Logfile, NewMsg ++ "\n");
    true ->
      werkzeug:logging(Logfile, FormattedMessage ++ "\n")
  end.

start_reading(true, _Logfile, _ReaderNNrs, _ServerPID) -> ok;

start_reading(false, Logfile, ReaderNNrs, ServerPID) ->
  ServerPID ! {self(), getmessages},
  receive
    {reply, Message, Terminated} ->
      write_message(Message, Logfile, ReaderNNrs),
      start_reading(Terminated, Logfile, ReaderNNrs, ServerPID);
    terminate ->
      ok
  end.