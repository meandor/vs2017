%%%-------------------------------------------------------------------
%%% @doc
%%% Starts ggT-Processes with configurations from the coordinator and
%%% its own config file.
%%%
%%% For a more detailed view see documentation section 3.1
%%% @end
%%%-------------------------------------------------------------------
-module(starter).

-export([start/1, log/2]).

log(Config, Message) ->
  {ok, StarterID} = werkzeug:get_config_value(starterid, Config),
  Logfile = list_to_atom(lists:concat(["ggt", integer_to_list(StarterID), "@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  werkzeug:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

request_steering_values(ConfigPath, CoordinatorName, GroupTeam, StarterNumber, NameService) ->
  CoordinatorName ! {self(), getsteeringval},
  receive
    {steeringval, WorkingTime, TerminationTime, Quota, GGTProcessNumber} ->
      startGGT(WorkingTime, TerminationTime, Quota, GGTProcessNumber, GroupTeam, StarterNumber, CoordinatorName, ConfigPath, NameService)
  end
.

startGGT(WorkingTime, TerminationTime, Quota, 0, GroupTeam, StartNumber, CoordinatorName, Config, NameService) -> ok;

startGGT(WorkingTime, TerminationTime, Quota, GGTProcessNumber, GroupTeam, StartNumber, CoordinatorName, ConfigPath, Nameservice) ->
%<PraktikumsgruppenID><TeamID><Nummer des ggT-Prozess><Nummer des Starters>
  GGTProcessName = erlang:list_to_atom(lists:concat([atom_to_list(GroupTeam), integer_to_list(GGTProcessNumber), atom_to_list(StartNumber)])),
  Logging = log(ConfigPath),
  werkzeug:logging(Logging, lists:concat(["starter>>", GGTProcessName, " started \n"])),
  ggt:start(WorkingTime, TerminationTime, Quota, GGTProcessName, CoordinatorName, Nameservice),

% Der Starter startet die ggT-Prozesse mit den zugehörigen Werten:
% der Verzögerungszeit, die Terminierungszeit,
% der Startnummer dieses Prozesses (also der wievielte gestartete ggT-Prozess er ist),
% seine eindeutige Starternummer, die Praktikumsgruppennummer,
% die Teamnummer sowie die benötigten Kontaktdaten für den Namensdienst
% und den Koordinator und die Abstimmungsquote als konkrete Anzahl.

  startGGT(WorkingTime, TerminationTime, Quota, GGTProcessNumber - 1, GroupTeam, StartNumber, CoordinatorName, ConfigPath, Nameservice).

%% Starts the starter with unique starterID
start(StarterID) -> start(StarterID, "./config/ggt.cfg").
start(StarterID, ConfigPath) ->
  Logging = log(ConfigPath),
  werkzeug:logging(Logging, lists:concat(["starter>>", ConfigPath, " opened \n"])),

  %Der Starter liest aus der Datei ggt.cfg die weiteren Werte aus:
  % die Erlang-Node des Namensdienstes, der Name des Koordinators,
  % die Nummer der Praktikumsgruppe und die Nummer des Teams.

  Config = werkzeug:loadConfig(ConfigPath),
  {ok, NSNode} = werkzeug:get_config_value(nameservicenode, Config),
  {ok, NSName} = werkzeug:get_config_value(nameservicename, Config),

  %TODO figure out ping?
  NameService = {NSName, NSNode},
  %pong = net_adm:ping(NameService),
  NameService ! {self(), {rebind, StarterID, node()}},

  {ok, CoordinatorName} = werkzeug:get_config_value(koordinatorname, Config),
  {ok, GroupNumber} = werkzeug:get_config_value(praktikumsgruppe, Config),
  {ok, TeamNumber} = werkzeug:get_config_value(teamnummer, Config),
  GroupTeam = erlang:list_to_atom(lists:concat([integer_to_list(GroupNumber), integer_to_list(TeamNumber)])),
  request_steering_values(ConfigPath, CoordinatorName, GroupTeam, StarterID, NameService).


log(ConfigPath) ->
  erlang:error(not_implemented).