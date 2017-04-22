%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(starter).


-export([start/1]).

loggingAtom(Config) ->
  {ok, Name} = werkzeug:get_config_value(koordinatorname, Config),
  LogfileName = lists:concat(["Koordinator@", Name, ".log"]),
  erlang:list_to_atom(LogfileName).


% Starter hat eindeutige Nummer, Beim starten des Starters wird ihm seine Starternummer mitgegeben.
start(StarterNumber) -> start(StarterNumber, "./config/ggt.cfg").

start(StarterNumber, ConfigPath) ->

  Config = werkzeug:loadConfig(ConfigPath),
  Logging = loggingAtom(Config),
  werkzeug:logging(Logging, lists:concat(["starter>>" , ConfigPath, " opened \n"])),

  %Der Starter liest aus der Datei ggt.cfg die weiteren Werte aus:
  % die Erlang-Node des Namensdienstes, der Name des Koordinators,
  % die Nummer der Praktikumsgruppe und die Nummer des Teams.

  {ok, NSNode} = werkzeug:get_config_value(nameservicenode, Config),
  {ok, NSName} = werkzeug:get_config_value(nameservicename, Config),

  %TODO figure out ping?
  NameService = {NSName, NSNode},
  %pong = net_adm:ping(NameService),
  NameService  ! {self(), {rebind, StarterNumber, node()}},

  {ok, CoordinatorName} = werkzeug:get_config_value(koordinatorname, Config),
  {ok, GroupNumber} = werkzeug:get_config_value(praktikumsgruppe, Config),
  {ok, TeamNumber} = werkzeug:get_config_value(teamnummer, Config),
  GroupTeam = erlang:list_to_atom([GroupNumber, TeamNumber]),
  request_steering_values(ConfigPath, CoordinatorName, GroupTeam, StarterNumber, NameService).

request_steering_values(ConfigPath, CoordinatorName, GroupTeam, StarterNumber, NameService) ->
  CoordinatorName ! {self(), getsteeringval},
  receive
    {steeringval,WorkingTime,TerminationTime,Quota,GGTProcessNumber} ->
      startGGT(WorkingTime, TerminationTime, Quota, GGTProcessNumber, GroupTeam, StarterNumber, CoordinatorName, ConfigPath, NameService)
  end
.

startGGT(WorkingTime,TerminationTime,Quota,0, GroupTeam, StartNumber, CoordinatorName, Config, NameService) -> ok;

startGGT(WorkingTime,TerminationTime,Quota,GGTProcessNumber, GroupTeam, StartNumber,CoordinatorName, ConfigPath, Nameservice) ->
  %<PraktikumsgruppenID><TeamID><Nummer des ggT-Prozess><Nummer des Starters>
  %GGTProcessName = erlang:list_to_atom([atom_to_list(GroupTeam), GGTProcessNumber, atom_to_list(StartNumber)]),
  GGTProcessName =  list_to_atom("c1234"),
  ggt:start(WorkingTime, TerminationTime, Quota, GGTProcessName, CoordinatorName, Nameservice),

  % Der Starter startet die ggT-Prozesse mit den zugehörigen Werten:
  % der Verzögerungszeit, die Terminierungszeit,
  % der Startnummer dieses Prozesses (also der wievielte gestartete ggT-Prozess er ist),
  % seine eindeutige Starternummer, die Praktikumsgruppennummer,
  % die Teamnummer sowie die benötigten Kontaktdaten für den Namensdienst
  % und den Koordinator und die Abstimmungsquote als konkrete Anzahl.

  startGGT(WorkingTime, TerminationTime, Quota, GGTProcessNumber - 1, GroupTeam, StartNumber,CoordinatorName, ConfigPath, Nameservice).