-module(cmem).

-export([initCMEM/2, delCMEM/1, updateClient/4, getClientNNr/2]).

% Initialisieren des CMEM
initCMEM(RemTime, Datei) ->
  werkzeug:logging(Datei, lists:concat(["CMEM>>> initialized with ", RemTime, " ms remaining time\n"])),
  [[], RemTime].

% Löschen des CMEM
delCMEM(_CMEM) -> ok.

% Speichern/Aktualisieren eines Clients in dem CMEM
updateClient([CMEMList, RemTime], ClientID, NNr, Datei) ->
  ClientTS = werkzeug:getUTC(),
  werkzeug:logging(Datei, lists:concat(["CMEM>>> Client ", ClientID, " updated (", NNr, "/", ClientTS, ")\n"])),
  [lists:keystore(ClientID, 1, CMEMList, {ClientID, NNr, ClientTS}), RemTime].

% Abfrage welche Nachrichtennummer der Client als nächstes erhalten darf
getClientNNr([CMEMList, RemTime], ClientID) ->
  Existent = lists:keymember(ClientID, 1, CMEMList),
  getClientNNr([CMEMList, RemTime], ClientID, Existent).

% Unbekannter Client
getClientNNr(_CMEM, _ClientID, false) -> 1;

% Bekannter Client, Zeit abgelaufen? JA -> 1, NEIN, NNr + 1
getClientNNr([CMEMList, RemTime], ClientID, true) ->
  {ClientID, NNr, ClientTimestamp} = lists:keyfind(ClientID, 1, CMEMList),
  Duration = ClientTimestamp + RemTime,
  Now = werkzeug:getUTC(),
  if
    Duration >= Now -> NNr + 1;
    true -> 1
  end.