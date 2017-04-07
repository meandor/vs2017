-module(hbq_tests).
-include_lib("eunit/include/eunit.hrl").

-export([testReplyOKServer/0, testReplyMSGNumberServer/0]).

testReplyOKServer() ->
  receive
    {reply, ok} -> ok
  end.

testReplyMSGNumberServer() ->
  receive
    {reply, Number} ->
      if
        is_integer(Number) -> ok
      end
  end.

initHBQ(ServerPID) ->
  HBQPID = hbq:start(),
  HBQPID ! {ServerPID, {request, initHBQ}}, HBQPID.

initHBQ_test() ->
  ServerPID = spawn(?MODULE, testReplyOKServer, []),
  initHBQ(ServerPID),
  timer:sleep(1000),
  undefined = erlang:process_info(ServerPID).

push_HBQ_test() ->
  ServerPID = spawn(?MODULE, testReplyOKServer, []),
  HBQPID = initHBQ(ServerPID),
  HBQPID ! {ServerPID,  {pushHBQ, [1,"Bla",erlang:now()]}},
  timer:sleep(1000),
  undefined = erlang:process_info(ServerPID).

deliverMSG_test() ->
  ServerPID = spawn(?MODULE, testReplyMSGNumberServer, []),
  HBQPID = initHBQ(ServerPID),
  HBQPID ! {ServerPID,  {pushHBQ, [2,"Bla",erlang:now()]}},
  HBQPID ! {ServerPID, {request, deliverMSG, 2, ServerPID}},
  timer:sleep(1000),
  undefined = erlang:process_info(ServerPID).

msglistToTuple_test() ->
  [{3, "Bla", 0 , 0}, {1, "Bla", 0 , 0}, {2, "Bla", 0, 0}] =
    hbq:apply_on_list([[3, "Bla", 0 , 0], [1, "Bla", 0 , 0], [2, "Bla", 0, 0]], [], fun list_to_tuple/1).

sort_hbq_test() ->
  [[1,"Bla",0,0],[2,"Bla",0,0],[3,"Bla",0,0]] = hbq:sort([[3, "Bla", 0 , 0], [1, "Bla", 0 , 0], [2, "Bla", 0, 0]]).

sort_hbq_empty_test() ->
  [] = hbq:sort([]).