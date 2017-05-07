%%%-------------------------------------------------------------------
%%% @doc
%%% Starts ggT-Processes with configurations from the coordinator and
%%% its own config file.
%%%
%%% For a more detailed view see documentation section 3.1
%%% @end
%%%-------------------------------------------------------------------
-module(starter).

-export([start/1, log/2, discover_coordinator/2, get_steering_values/2, ggT_id/4, start_ggT_processes/4]).

log(Config, Message) ->
  {ok, StarterID} = werkzeug:get_config_value(starterid, Config),
  Logfile = list_to_atom(lists:concat(["ggt", integer_to_list(StarterID), "@", atom_to_list(node()), ".log"])),
  FullMessage = Message ++ ["\n"],
  werkzeug:logging(Logfile, lists:concat(FullMessage)),
  Logfile.

discover_coordinator(NameService, Config) ->
  {ok, CoordinatorName} = werkzeug:get_config_value(koordinatorname, Config),
  NameService ! {self(), {lookup, CoordinatorName}},
  receive
    not_found ->
      log(Config, ["service ", atom_to_list(CoordinatorName), " not found..."]),
      {};
    {pin, {Name, Node}} ->
      pong = net_adm:ping(Node),
      log(Config, ["coordinator service ", atom_to_list(Name), "(", atom_to_list(Node), ")", " bound..."]),
      {Name, Node}
  end.

get_steering_values(Coordinator, Config) ->
  Coordinator ! {self(), getsteeringval},
  receive
    {steeringval, ArbeitsZeit, TermZeit, Quota, GGTProzessnummer} ->
      log(Config, ["getsteeringval: ", integer_to_list(ArbeitsZeit), " work time; ", integer_to_list(TermZeit), " term time; ", integer_to_list(Quota), " quota; ", integer_to_list(GGTProzessnummer), " #ggT proccesses."]),
      [ArbeitsZeit, TermZeit, Quota, GGTProzessnummer]
  end.

ggT_id(GroupNumber, TeamNumber, GgTProcessNumber, StarterID) ->
  String = lists:concat([integer_to_list(GroupNumber), integer_to_list(TeamNumber), integer_to_list(GgTProcessNumber), integer_to_list(StarterID)]),
  list_to_atom(String).

-spec start_ggT_processes(integer(), map(), fun(), list()) -> any().
start_ggT_processes(0, _ParamMap, _Fun, Result) -> Result;
start_ggT_processes(NumberOfGgtProcesses, ParamMap, Fun, Result) ->
  GgTID = ggT_id(maps:get(groupnumber, ParamMap), maps:get(teamnumber, ParamMap), NumberOfGgtProcesses, maps:get(starterid, ParamMap)),
  NewResult = Result ++ [Fun(maps:get(worktime, ParamMap), maps:get(termtime, ParamMap), maps:get(quota, ParamMap), GgTID, maps:get(coordinator, ParamMap), maps:get(nameservice, ParamMap))],
  start_ggT_processes(NumberOfGgtProcesses - 1, ParamMap, Fun, NewResult).

%% Starts the starter with unique starterID
start(StarterID) -> start(StarterID, "./config/ggt.cfg").
start(StarterID, ConfigPath) ->
  Config = werkzeug:loadConfig(ConfigPath),
  NewConfig = lists:concat([Config, [{starterid, StarterID}]]),
  log(NewConfig, ["Starttime: ", werkzeug:timeMilliSecond(), " with PID ", pid_to_list(self())]),
  log(NewConfig, [ConfigPath, " opened..."]),

  {ok, GroupNumber} = werkzeug:get_config_value(praktikumsgruppe, Config),
  {ok, TeamNumber} = werkzeug:get_config_value(teamnummer, Config),
  log(NewConfig, [ConfigPath, " loaded..."]),

  NameService = utils:bind_nameservice(NewConfig),
  log(NewConfig, ["Nameservice '", pid_to_list(NameService), "' bound..."]),

  Coordinator = discover_coordinator(NameService, NewConfig),
  [ArbeitsZeit, TermZeit, Quota, NumberOfGgtProcesses] = get_steering_values(Coordinator, NewConfig),
  ParamMap = #{worktime => ArbeitsZeit, termtime => TermZeit, starterid => StarterID, groupnumber => GroupNumber, teamnumber => TeamNumber, nameservice => NameService, coordinator => Coordinator, quota => Quota},
  start_ggT_processes(NumberOfGgtProcesses, ParamMap, fun ggt:start/6, []).
