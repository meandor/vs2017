%%%-------------------------------------------------------------------
%%% @doc
%%% Starts ggT-Processes with configurations from the coordinator and
%%% its own config file.
%%%
%%% For a more detailed view see documentation section 3.1
%%% @end
%%%-------------------------------------------------------------------
-module(starter).

-export([start/1, log/2, bindNameService/1]).

log(Config, Message) ->
  {ok, StarterID} = werkzeug:get_config_value(starterid, Config),
  Logfile = list_to_atom(lists:concat(["ggt", integer_to_list(StarterID), "@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  werkzeug:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

bindNameService(Config) ->
  {ok, NSNode} = werkzeug:get_config_value(nameservicenode, Config),
  {ok, NSName} = werkzeug:get_config_value(nameservicename, Config),
  pong = net_adm:ping(NSNode),
  NameService = global:whereis_name(NSName),
  log(Config, ["Nameservice '", pid_to_list(NameService), "' bound..."]),
  NameService.

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
%% Koordinator chef (chef) gebunden.
%% getsteeringval: 2 Arbeitszeit ggT; 42 Wartezeit Terminierung ggT; 7 Abstimmungsquote ggT; 9-te GGT Prozess.
start(StarterID, ConfigPath) ->
  Config = werkzeug:loadConfig(ConfigPath),
  NewConfig = lists:concat([Config, [{starterid, StarterID}]]),
  log(NewConfig, ["Starttime: ", werkzeug:timeMilliSecond(), " with PID ", atom_to_list(self())]),
  log(NewConfig, [ConfigPath, " opened..."]),

  {ok, CoordinatorName} = werkzeug:get_config_value(koordinatorname, Config),
  {ok, GroupNumber} = werkzeug:get_config_value(praktikumsgruppe, Config),
  {ok, TeamNumber} = werkzeug:get_config_value(teamnummer, Config),
  log(NewConfig, [ConfigPath, " loaded..."]),

  NameService = bindNameService(NewConfig),

  NameService ! {self(), {lookup, CoordinatorName}},
  receive
    not_found ->
      log(NewConfig, ["service ", atom_to_list(CoordinatorName), " not found..."]);
    {pin, {Name, Node}} ->
      log(NewConfig, ["coordinator service ", atom_to_list(Name), "(", atom_to_list(Node), ")", " bound..."])
  end,


  GroupTeam = erlang:list_to_atom(lists:concat([integer_to_list(GroupNumber), integer_to_list(TeamNumber)])),
  request_steering_values(ConfigPath, CoordinatorName, GroupTeam, StarterID, NameService).


log(ConfigPath) ->
  erlang:error(not_implemented).