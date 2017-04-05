-module(hbq_tests).
-include_lib("eunit/include/eunit.hrl").

-export([testReplyOKServer/0]).

testReplyOKServer() ->
  receive
    {reply, ok} -> ok
  end.

initHBQ_test() ->
  ServerPID = spawn(?MODULE, testReplyOKServer, []),
  HBQPID = hbq:start(),
  HBQPID ! {ServerPID, {request, initHBQ}},
  timer:sleep(2000),
  undefined = erlang:process_info(ServerPID).
