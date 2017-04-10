
-module(client_tests).
-export([]).

-include_lib("eunit/include/eunit.hrl").


send_receive_test() ->
  ServerPID = server:startMe(),
  client:startClient(1000 * 60, ServerPID),
  ServerPID ! terminate.
