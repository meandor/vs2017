-module(hbq_tests).
-include_lib("eunit/include/eunit.hrl").

-export([testReplyOKServer/0]).

testReplyOKServer() ->
  receive
    {reply, ok} -> ok
  end.

%initHBQ_test() ->
%  ServerPID = spawn(?MODULE, testReplyOKServer, []),
%  HBQPID = hbq:start(),
%  HBQPID ! {ServerPID, {request, initHBQ}},
%  timer:sleep(2000),
%  undefined = erlang:process_info(ServerPID).

msglistToTuple_test() ->
  [{3, "Bla", 0 , 0}, {1, "Bla", 0 , 0}, {2, "Bla", 0, 0}] = hbq:apply_on_list([[3, "Bla", 0 , 0], [1, "Bla", 0 , 0], [2, "Bla", 0, 0]], [], fun list_to_tuple/1).

sort_hbq_test() ->
 % io:format(hbq:sort([[3, "Bla", 0 , 0], [1, "Bla", 0 , 0], [2, "Bla", 0, 0]])).
  [[1,"Bla",0,0],[2,"Bla",0,0],[3,"Bla",0,0]] = hbq:sort([[3, "Bla", 0 , 0], [1, "Bla", 0 , 0], [2, "Bla", 0, 0]]).