-module(hbq_tests).
-include_lib("eunit/include/eunit.hrl").

-export([testReplyOKServer/0, testReplyMSGNumberServer/1, testReplyMSGNumber9Server/0]).

testReplyOKServer() ->
  receive
    {reply, ok} -> ok
  end.

testReplyMSGNumberServer(Number) ->
  receive
    {reply, ok} -> ok;
    {reply, Number} -> ok
  end.


testReplyMSGNumber9Server() ->
  receive
    {reply, ok} -> testReplyMSGNumber9Server();
    {reply, 0} -> ok
  end.

initHBQ(ServerPID) ->
  HBQPID = hbq:start(),
  HBQPID ! {ServerPID, {request, initHBQ}}, HBQPID.

%%initHBQ_test() ->
%%  ServerPID = spawn(?MODULE, testReplyOKServer, []),
%%  initHBQ(ServerPID),
%%  timer:sleep(1000),
%%  ?assert(undefined =:= erlang:process_info(ServerPID)).
%%
%%push_HBQ_test() ->
%%  ServerPID = spawn(?MODULE, testReplyOKServer, []),
%%  HBQPID = initHBQ(ServerPID),
%%  HBQPID ! {ServerPID,  {pushHBQ, [1,"Bla",erlang:now()]}},
%%  timer:sleep(1000),
%%  ?assert(undefined =:= erlang:process_info(ServerPID)).

deliverMSG_test() ->
  ServerPID = spawn(?MODULE, testReplyMSGNumberServer, [0]),
  HBQPID = initHBQ(ServerPID),
  ?assert(undefined =/= erlang:process_info(HBQPID)),
  HBQPID ! {ServerPID,  {pushHBQ, [0,"Bla",123]}}.
%%  HBQPID ! {ServerPID, {request, deliverMSG, 0, ServerPID}},
%%  timer:sleep(2000),
%%  ?assert(undefined =:= erlang:process_info(ServerPID)).

%%fillTest_test() ->
%%  ServerPID = spawn(?MODULE, testReplyMSGNumber9Server, []),
%%  HBQPID = initHBQ(ServerPID),
%% % HBQPID ! {ServerPID,  {pushHBQ, [0,"Bla",erlang:now()]}},
%%  HBQPID ! {ServerPID,  {pushHBQ, [5,"Bla",erlang:now()]}},
%%  HBQPID ! {ServerPID,  {pushHBQ, [6,"Bla",erlang:now()]}},
%%  HBQPID ! {ServerPID,  {pushHBQ, [7,"Bla",erlang:now()]}},
%%  HBQPID ! {ServerPID,  {pushHBQ, [8,"Bla",erlang:now()]}},
%%  HBQPID ! {ServerPID,  {pushHBQ, [9,"Bla",erlang:now()]}},
%%  HBQPID ! {ServerPID, {request, deliverMSG, 0, ServerPID}},
%%  timer:sleep(1000),
%%  ?assert(undefined =:= erlang:process_info(ServerPID)).
%%
%%
%%msglistToTuple_test() ->
%%  [{3, "Bla", 0 , 0}, {1, "Bla", 0 , 0}, {2, "Bla", 0, 0}] =
%%    hbq:apply_on_list([[3, "Bla", 0 , 0], [1, "Bla", 0 , 0], [2, "Bla", 0, 0]], [], fun list_to_tuple/1).
%%
%%sort_hbq_test() ->
%%  [[1,"Bla",0,0],[2,"Bla",0,0],[3,"Bla",0,0]] = hbq:sort([[3, "Bla", 0 , 0], [1, "Bla", 0 , 0], [2, "Bla", 0, 0]]).
%%
%%sort_hbq_empty_test() ->
%%  [] = hbq:sort([]).