%%%-------------------------------------------------------------------
%%% @doc
%%% Detailed Documentation: See section 3.6 of docs/aufgabe1_dokumentation.pdf
%%%
%%% This module receives incoming messages and validates them before it puts them
%%% into the DLQ. Invalid messages are hold back. A valid message is a message,
%%%  that is expected by the DLQ.Any other message is invalid.
%%% For Details about the error handling process see docs/aufgabe1_dokumentation.pdf
%%% section 3.6.3.
%%% @end
%%%-------------------------------------------------------------------
-module(hbq).
-export([start/0, start/1, hbq/3, apply_on_list/3, sort/1]).

loadConfig(Configfile) ->
  {ok, Config} = file:consult(Configfile),
  Config.

log(Config, Message) ->
  {ok, NodeName} = werkzeug:get_config_value(hbqnode, Config),
  LogfileName = lists:concat(["HB-DLQ@", NodeName, ".log"]),
  LogAtom = erlang:list_to_atom(LogfileName),
  werkzeug:logging(LogAtom, lists:concat(Message)),
  LogAtom.

%% Used to apply a function on a list. Helper function for sorting.
apply_on_list([H | T], X, Func) ->
  apply_on_list(T, lists:append(X, [Func(H)]), Func);
apply_on_list([], X, _) -> X.

%% Sorts messages in the HBQ
sort(Messages) ->
  apply_on_list(lists:keysort(1, apply_on_list(Messages, [], fun list_to_tuple/1)), [], fun tuple_to_list/1).

%% Inserts an error message into dlq
insertErrorMessage(ErrorNNr, DLQ, Config) ->
  From = dlq:expectedNr(DLQ),
  ErrorMsg = "Fehlernachricht fuer Nachrichtennummern " ++ integer_to_list(From) ++ " bis " ++ integer_to_list(ErrorNNr),
  ErrorMessage = [ErrorNNr, ErrorMsg, erlang:now(), erlang:now()],
  dlq:push2DLQ(ErrorMessage, DLQ, Config).

%% Handles inserting error message or doing nothing if everything is ok!
handleFaultyHBQ([HBQMessage | HBQRest], [DLQMessages, DLQSize], Config) ->
  HBQLength = length([HBQMessage | HBQRest]),
  [NNr, _MSG, _TSclientin, _TShbqin] = HBQMessage,
  if
    HBQLength >= (DLQSize * 2 / 3) ->
      NewDLQ = insertErrorMessage(NNr - 1, [DLQMessages, DLQSize], Config),
      startPushing(HBQMessage, HBQRest, NewDLQ, Config);
    true ->
      {[HBQMessage | HBQRest], [DLQMessages, DLQSize]}
  end.

%% push only valid messages into dlq, message is valid if it is expected
pushToDLQ([NNr, Msg, TSclientout, TShbqin], DLQ, Logfile) ->
  ExpectedNNr = dlq:expectedNr(DLQ),
  if
    ExpectedNNr =:= NNr ->
      dlq:push2DLQ([NNr, Msg, TSclientout, TShbqin], DLQ, Logfile);
    true ->
      DLQ
  end.

%% Push valid message into dlq directly
startPushing([NNr, Msg, TSclientout, TShbqin], HBQ, DLQ, Config) ->
  Logfile = log(Config, ["HBQ>>> pushing message: ", NNr, "\n"]),
  MSGInsertedDLQ = pushToDLQ([NNr, Msg, TSclientout, TShbqin], DLQ, Logfile),
  if
    DLQ =:= MSGInsertedDLQ -> % DLQ did not change, put the message into the hbq
      NewHBQ = sort(HBQ ++ [[NNr, Msg, TSclientout, TShbqin]]),
      handleFaultyHBQ(NewHBQ, DLQ, Config);
    true -> % DLQ did change, hbq stays the same, maybe we can put more stuff into the dlq
      if
        HBQ =/= [] ->
          [Message | HBQRest] = HBQ,
          startPushing(Message, HBQRest, MSGInsertedDLQ, Config);
        true ->
          {HBQ, MSGInsertedDLQ}
      end
  end.

%% HBQ interface. For detailed description see section 3.6.2 in docs/aufgabe1_dokumentation.pdf
hbq(HBQ, DLQ, Config) ->
  receive
  % Initializes the HBQ with empty value and sends reply to server
    {ServerPID, {request, initHBQ}} ->
      Logfile = log(Config, ["HBQ>>> initialized by ", pid_to_list(ServerPID), "\n"]),
      {ok, DlqLimit} = werkzeug:get_config_value(dlqlimit, Config),
      ServerPID ! {reply, ok},
      hbq([], dlq:initDLQ(DlqLimit, Logfile), Config);

  %Forwards the command to deliver a message to the PID "ToClient" to the dlq
    {ServerPID, {request, deliverMSG, NNr, ToClient}} ->
      {ok, NodeName} = werkzeug:get_config_value(hbqnode, Config),
      LogfileName = lists:concat(["HB-DLQ@", NodeName, ".log"]),
      Logfile = erlang:list_to_atom(LogfileName),
      log(Config, ["HBQ>>> dlq:delivermsg", NNr, pid_to_list(ToClient), "\n"]),
      SendNNr = dlq:deliverMSG(NNr, ToClient, DLQ, Logfile),
      ServerPID ! {reply, SendNNr},
      hbq(HBQ, DLQ, Config);

  % Terminates the process and sends an ok to the server.
    {ServerPID, {request, dellHBQ}} ->
      ServerPID ! {reply, ok},
      ok;

  % start the push process
    {ServerPID, {request, pushHBQ, Message}} ->
      {HBQNew, DLQNew} = startPushing(Message ++ [erlang:now()], HBQ, DLQ, Config),
      ServerPID ! {reply, ok},
      hbq(HBQNew, DLQNew, Config)
  end.

%% Used to start the HBQ with default config file
start() ->
  start("./config/server.cfg").

%% Used for starting the HBQ with a custom config file
start(Configfile) ->
  Config = loadConfig(Configfile),
  {ok, HBQName} = werkzeug:get_config_value(hbqname, Config),
  log(Config, ["HBQ>>> server.cfg opened \n"]),
  HBQPID = spawn(?MODULE, hbq, [[], [], Config]),
  register(HBQName, HBQPID).
