-module(hbq).
-export([start/0, hbq/4, apply_on_list/3, sort/1]).

loadConfig() ->
  {ok, Config} = file:consult("./config/server.cfg"),
  Config.

log(Config, Message) ->
  {ok, NodeName} = werkzeug:get_config_value(hbqnode, Config),
  LogfileName = lists:concat(["HB-DLQ@", NodeName, ".log"]),
  LogAtom = erlang:list_to_atom(LogfileName),
  werkzeug:logging(LogAtom, lists:concat(Message)),
  LogAtom.

hbq(HBQ, ExpectedNNr, DLQ, Config) ->
  receive

    % Initializes the HBQ with empty value and sends reply to server
    {ServerPID, {request, initHBQ}} ->
      Logfile = log(Config, ["HBQ>>> initialized by ", pid_to_list(ServerPID), "\n"]),
      {ok, DlqLimit} = werkzeug:get_config_value(dlqlimit, Config),
      ServerPID ! {reply, ok},
      hbq([], 0, dlq:initDLQ(DlqLimit, Logfile), Config);

    %Forwards the command to deliver a message to the PID "ToClient" to the dlq
    {ServerPID, {request, deliverMSG, NNr, ToClient}} ->
        Logfile = log(Config, ["HBQ>>> delivered", 1, "\n"]),
        Number = dlq:deliverMSG(NNr, ToClient, DLQ, Logfile),
        ServerPID ! {reply, Number};

    % Terminates the process and sends an ok to the server.
    {ServerPID, {request,dellHBQ}} ->
          ServerPID ! {reply, ok},
          exit("dellHBQ was called");

    %Pushes a message to the HBQ, if its not the expected one.
    %After each push the hbq gets sorted and inspected for the expected message number at the beginning.
    {ServerPID, {pushHBQ, [NNr,Msg,TSclientout]}} ->
      Logfile = log(Config, ["HBQ>>> pushing message: ", NNr, "\n"]),
      [_, DLQSize] = DLQ,
      if NNr > ExpectedNNr ->
          HBQ = lists:append(HBQ, [[NNr, Msg, TSclientout, erlang:now()]]),
          HBQ = sort(HBQ),
          {ExpectedNNr, HBQ} = pushAllConsecutiveSequenceNumbers(HBQ, DLQ, Logfile, ExpectedNNr),
          hbq(HBQ, ExpectedNNr, DLQ, Config);
         NNr == ExpectedNNr ->
          dlq:push2DLQ([NNr, Msg, TSclientout, erlang:now()], DLQ, "Test");
        length(HBQ) >= DLQSize * (2 / 3) ->
          HBQ = lists:append(HBQ, [[NNr, Msg, TSclientout, erlang:now()]]),

          ok
      end,
      ServerPID ! {reply, ok}

  end
.

apply_on_list([H | T], X, Func) ->
  apply_on_list(T, lists:append(X, [Func(H)]), Func);
apply_on_list([], X, _) -> X.

sort(Messages) -> apply_on_list(lists:keysort(1, apply_on_list(Messages, [], fun list_to_tuple/1)), [], fun tuple_to_list/1).

pushAllConsecutiveSequenceNumbers([[NNr, MSG, TS1, TS2] | Tail], DLQ, Datei, ExpectedNNr) ->
  Fehler = string:str(MSG, "Fehlernachricht"),
  if NNr == ExpectedNNr or Fehler  ->
    dlq:push2DLQ(NNr, DLQ, Datei),
    pushAllConsecutiveSequenceNumbers(Tail, DLQ, Datei, ExpectedNNr + 1)
  end,
  werkzeug:logging(Datei, lists:concat(["HBQ>>>sent all messages to dlq until NNR: ", NNr, "\n"])),
  {ExpectedNNr, [[NNr, MSG, TS1, TS2] | Tail]}
.




start() ->
  Config = loadConfig(),
  log(Config, ["HBQ>>> server.cfg opened \n"]),
  spawn(?MODULE, hbq, [[], 0, [], Config]).




