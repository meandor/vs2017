-module(hbq_tests).
-include_lib("eunit/include/eunit.hrl").

-export([testReplyOKServer/1, testReplyMSGNumberServer/1]).

testReplyOKServer(0) -> ok;

testReplyOKServer(Times) ->
  receive
    {reply, ok} ->
      Less = Times - 1,
      testReplyOKServer(Less)
  end.

testReplyMSGNumberServer(Number) ->
  receive
    {reply, ok} ->
      testReplyMSGNumberServer(Number);
    {reply, Number} -> ok
  end.

initHBQ(ServerPID) ->
  initHBQ(ServerPID, "./config/server.cfg").

initHBQ(ServerPID, Configfile) ->
  HBQPID = hbq:start(Configfile),
  HBQPID ! {ServerPID, {request, initHBQ}},
  HBQPID.

%%initHBQ_test() ->
%%  ServerPID = spawn(?MODULE, testReplyOKServer, [1]),
%%  initHBQ(ServerPID),
%%  timer:sleep(100),
%%  ?assert(undefined =:= erlang:process_info(ServerPID)).
%%
%%push_one_message_into_empty_hbq_and_dlq_test() ->
%%  ServerPID = spawn(?MODULE, testReplyOKServer, [1]),
%%  ServerPID2 = spawn(?MODULE, testReplyOKServer, [1]),
%%  HBQPID = initHBQ(ServerPID),
%%  HBQPID ! {ServerPID, {pushHBQ, [1, "Bla", erlang:now()]}},
%%  timer:sleep(100),
%%  ?assert(undefined =:= erlang:process_info(ServerPID)),
%%  HBQPID ! {ServerPID2, {request, dellHBQ}},
%%  timer:sleep(100),
%%  ?assert(undefined =:= erlang:process_info(ServerPID2)),
%%  ?assert(undefined =:= erlang:process_info(HBQPID)).
%%
%%push_one_invalid_message_into_empty_hbq_test() ->
%%  ServerPID = spawn(?MODULE, testReplyOKServer, [1]),
%%  ServerPID2 = spawn(?MODULE, testReplyOKServer, [1]),
%%  HBQPID = initHBQ(ServerPID),
%%  HBQPID ! {ServerPID, {pushHBQ, [3, "Bla2", erlang:now()]}},
%%  timer:sleep(100),
%%  ?assert(undefined =:= erlang:process_info(ServerPID)),
%%  HBQPID ! {ServerPID2, {request, dellHBQ}},
%%  timer:sleep(100),
%%  ?assert(undefined =:= erlang:process_info(ServerPID2)),
%%  ?assert(undefined =:= erlang:process_info(HBQPID)).
%%
%%push_invalid_messages_into_hbq_test() ->
%%  ServerPID = spawn(?MODULE, testReplyOKServer, [6]),
%%  HBQPID = initHBQ(ServerPID, "./test-config/server.cfg"),
%%
%%  HBQPID ! {ServerPID, {pushHBQ, [1, "Bla2", erlang:now()]}},
%%  timer:sleep(100),
%%  ?assert(undefined =/= erlang:process_info(ServerPID)),
%%
%%  % Insert invalid messages
%%  HBQPID ! {ServerPID, {pushHBQ, [3, "Bla2", erlang:now()]}},
%%  timer:sleep(100),
%%  ?assert(undefined =/= erlang:process_info(ServerPID)),
%%  HBQPID ! {ServerPID, {pushHBQ, [4, "Bla2", erlang:now()]}},
%%  timer:sleep(100),
%%  ?assert(undefined =/= erlang:process_info(ServerPID)),
%%  HBQPID ! {ServerPID, {pushHBQ, [5, "Bla2", erlang:now()]}},
%%  timer:sleep(100),
%%  ?assert(undefined =/= erlang:process_info(ServerPID)),
%%
%%  HBQPID ! {ServerPID, {request, dellHBQ}},
%%  timer:sleep(100),
%%  ?assert(undefined =:= erlang:process_info(HBQPID)),
%%  ?assert(undefined =:= erlang:process_info(ServerPID)).

deliverMSG_test() ->
  ServerPID = spawn(?MODULE, testReplyMSGNumberServer, [0]),
  ClientPID = spawn(reader, start_reading, [false, 'test.log', [1, 3, 4], ServerPID]),
  HBQPID = initHBQ(ServerPID),

  HBQPID ! {ServerPID, {pushHBQ, [1, "Bla", 123]}},
  timer:sleep(100),
  HBQPID ! {ServerPID, {request, deliverMSG, 1, ClientPID}},
  timer:sleep(100),

  ?assert(undefined =:= erlang:process_info(ServerPID)),
  ?assert(undefined =:= erlang:process_info(ClientPID)),
  HBQPID ! {ServerPID, {request, dellHBQ}},
  timer:sleep(100),

  ?assert(undefined =:= erlang:process_info(HBQPID)).

%%fillTest_test() ->
%%  ServerPID = spawn(?MODULE, testReplyMSGNumber9Server, []),
%%  HBQPID = initHBQ(ServerPID),
%%  % HBQPID ! {ServerPID,  {pushHBQ, [0,"Bla",erlang:now()]}},
%%  HBQPID ! {ServerPID, {pushHBQ, [5, "Bla", erlang:now()]}},
%%  HBQPID ! {ServerPID, {pushHBQ, [6, "Bla", erlang:now()]}},
%%  HBQPID ! {ServerPID, {pushHBQ, [7, "Bla", erlang:now()]}},
%%  HBQPID ! {ServerPID, {pushHBQ, [8, "Bla", erlang:now()]}},
%%  HBQPID ! {ServerPID, {pushHBQ, [9, "Bla", erlang:now()]}},
%%  HBQPID ! {ServerPID, {request, deliverMSG, 0, ServerPID}},
%%  timer:sleep(1000),
%%  ?assert(undefined =:= erlang:process_info(ServerPID)).
%%
%%msglistToTuple_test() ->
%%  [{3, "Bla", 0, 0}, {1, "Bla", 0, 0}, {2, "Bla", 0, 0}] =
%%    hbq:apply_on_list([[3, "Bla", 0, 0], [1, "Bla", 0, 0], [2, "Bla", 0, 0]], [], fun list_to_tuple/1).
%%
%%sort_hbq_test() ->
%%  [[1, "Bla", 0, 0], [2, "Bla", 0, 0], [3, "Bla", 0, 0]] = hbq:sort([[3, "Bla", 0, 0], [1, "Bla", 0, 0], [2, "Bla", 0, 0]]).
%%
%%sort_hbq_empty_test() ->
%%  [] = hbq:sort([]).