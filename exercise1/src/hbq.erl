-module(hbq).
-export([start/0, hbq/4]).

loadConfig() ->
  {ok, Config} = file:consult("./config/server.cfg"),
  Config.

log(Config, Message) ->
  {ok, NodeName} = werkzeug:get_config_value(hbqnode, Config),
  LogfileName = lists:concat(["HB-DLQ@", NodeName, ".log"]),
  LogAtom = erlang:list_to_atom(LogfileName),
  werkzeug:logging(LogAtom, lists:concat(Message)),
  LogAtom.

hbq(HBQ, CurrentNNr, DLQ, Config) ->
  receive
    {ServerPID, {request, initHBQ}} ->
      Logfile = log(Config, ["HBQ>>> initialized by ", pid_to_list(ServerPID), "\n"]),
      {ok, DlqLimit} = werkzeug:get_config_value(dlqlimit, Config),
      ServerPID ! {reply, ok},
      hbq([], 0, dlq:initDLQ(DlqLimit, Logfile), Config)
%%    {deliverMSG, NNr, ToClient, Datei} ->
%%      dlq:deliverMSG(NNr, ToClient, [DLQ, HBQSize], Datei);
%%    {dellHBQ} ->
%%      exit("dellHBQ was called"), ok;
%%    {pushHBQ, {[NNr, Msg, TSclientout, TShbqin], Datei}} ->
%%      werkzeug:logging(Datei, lists:concat(["HBQ>>> pushing message: ", NNr, "\n"])),
%%      Messages = lists:append(Messages, [[NNr, Msg, TSclientout, TShbqin]]),
%%      Messages = lists:keysort(1, Messages),
%%      Size = length(Messages),
%%      if NNr == CurrentNNr ->
%%        {CurrentNNr, Messages} = pushAllConsecutiveSequenceNumbers(Messages, DLQ, Datei),
%%        loop(Messages, HBQSize, CurrentNNr + 1, DLQ);
%%        Size >= HBQSize * (2 / 3) ->
%%          ok
%%      end
  end
.


%%%deliverMSG(MSGNr, ClientPID, Queue, Datei)
%%pushAllConsecutiveSequenceNumbers([[NNr, MSG, _, _] | Tail], DLQ, Datei) ->
%%  dlq:push2DLQ(NNr, DLQ, Datei),
%%  [NNr2, _, _, _] = Tail,
%%  Fehler = string:str(MSG, "Fehlernachricht"),
%%  if NNr == NNr2 - 1 or Fehler ->
%%    pushAllConsecutiveSequenceNumbers(Tail, DLQ, Datei)
%%  end,
%%  werkzeug:logging(Datei, lists:concat(["HBQ>>>sent all messages to dlq until NNR: ", NNr, "\n"])),
%%  {NNr, Tail}
%%.

start() ->
  Config = loadConfig(),
  log(Config, ["HBQ>>> server.cfg opened \n"]),
  spawn(?MODULE, hbq, [[], 0, [], Config]).




