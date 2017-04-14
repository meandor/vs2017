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

  %TODO check if correct?
  NameService = {NSName, NSNode},
  net_adm:ping(NSNode),
  NameService  ! {self(), {rebind, StarterNumber, node()}},

  {ok, CoordinatorName} = werkzeug:get_config_value(koordinatorname, Config),
  {ok, GroupNumber} = werkzeug:get_config_value(praktikumsgruppe, Config),
  {ok, TeamNumber} = werkzeug:get_config_value(teamnummer, Config),
  GroupTeam = erlang:list_to_atom([GroupNumber, TeamNumber]),
  request_steering_values(Config, CoordinatorName, GroupTeam, StarterNumber).

request_steering_values(Config, CoordinatorName, GroupTeam, StarterNumber) ->
  CoordinatorName ! {self(), getsteeringval},
  receive
    {steeringval,ArbeitsZeit,TermZeit,Quota,GGTProzessnummer} -> startGGT(ArbeitsZeit, TermZeit, Quota, GGTProzessnummer, GroupTeam, StarterNumber)
  end
.

startGGT(ArbeitsZeit,TermZeit,Quota,0, GroupTeam, StartNumber) -> ok;

startGGT(ArbeitsZeit,TermZeit,Quota,GGTProzessnummer, GroupTeam, StartNumber) ->
  %<PraktikumsgruppenID><TeamID><Nummer des ggT-Prozess><Nummer des Starters>
  GGTProcessName = erlang:list_to_atom([GroupTeam, GGTProzessnummer, StartNumber]),
  %TODO: StartGGT Process with GGTProccessName and given values
  startGGT(ArbeitsZeit, TermZeit, Quota, GGTProzessnummer - 1, GroupTeam, StartNumber).