%%%-------------------------------------------------------------------
%%% @doc
%%% Detailed Documentation: See section 3.5 of docs/aufgabe1_dokumentation.pdf
%%%
%%% This module is used by the server to remember requesting clients. Each client is saved with a timestamp.
%%% If the last request exceeds the clientlifetime parameter in server.cfg the client is understood as forgotten
%%% @end
%%%-------------------------------------------------------------------
-module(cmem).

-export([initCMEM/2, delCMEM/1, updateClient/4, getClientNNr/2]).

%% Initialize the cmem
initCMEM(RemTime, Datei) ->
  werkzeug:logging(Datei, lists:concat(["CMEM>>> initialized with ", RemTime, " ms remaining time\n"])),
  [[], RemTime].

%% deletes the cmem
delCMEM(_CMEM) -> ok.

%% Safe/Update a client in the cmem
updateClient([CMEMList, RemTime], ClientID, NNr, Datei) ->
  ClientTS = werkzeug:getUTC(),
  werkzeug:logging(Datei, lists:concat(["CMEM>>> Client ", pid_to_list(ClientID), " updated (", NNr, "/", ClientTS, ")\n"])),
  [lists:keystore(ClientID, 1, CMEMList, {ClientID, NNr, ClientTS}), RemTime].

%% Request which NNr the client may obtain next
getClientNNr([CMEMList, RemTime], ClientID) ->
  Existent = lists:keymember(ClientID, 1, CMEMList),
  getClientNNr([CMEMList, RemTime], ClientID, Existent).

%% unknown Client
getClientNNr(_CMEM, _ClientID, false) -> 1;

%% known client, time exceeded? YES -> 1, NO, NNr + 1
getClientNNr([CMEMList, RemTime], ClientID, true) ->
  {ClientID, NNr, ClientTimestamp} = lists:keyfind(ClientID, 1, CMEMList),
  Duration = ClientTimestamp + RemTime,
  Now = werkzeug:getUTC(),
  if
    Duration >= Now -> NNr + 1;
    true -> 1
  end.