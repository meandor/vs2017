-module(cmem_tests).

-include_lib("eunit/include/eunit.hrl").

emptyCMEM() -> [[], 123].
oneClientCMEM() -> [[{"ClientPID", 3, 1337}], 123].

initCMEM_test() -> ?assert(emptyCMEM() =:= cmem:initCMEM(123, 'test.log')).

delCMEM_test() -> ?assert(ok =:= cmem:delCMEM([])).

updateClient_empty_cmem_test() ->
  [[{"ClientPID", 3, ClientTS}], 123] = cmem:updateClient(emptyCMEM(), "ClientPID", 3, 'test.log'),
  ClientTS > 0.

updateClient_already_existing_client_cmem_test() ->
  [[{"ClientPID", 7, ClientTS}], 123] = cmem:updateClient(oneClientCMEM(), "ClientPID", 7, 'test.log'),
  ClientTS > 1337.

updateClient_new_client_cmem_test() ->
  [[{"ClientPID", 3, 1337}, {"ClientPID2", 8, ClientTS}], 123] = cmem:updateClient(oneClientCMEM(), "ClientPID2", 8, 'test.log'),
  ClientTS > 0.

getClientNNr_test_() -> [
  ?_assert(1 =:= cmem:getClientNNr(emptyCMEM(), "ClientPID")), % completely new client in empty list
  ?_assert(1 =:= cmem:getClientNNr(oneClientCMEM(), "ClientPID2")), % new client in not empty list
  ?_assert(1 =:= cmem:getClientNNr(oneClientCMEM(), "ClientPID")), % known client already timed out
  ?_assert(4 =:= cmem:getClientNNr([[{"ClientPID", 3, 2491335622909}], 123], "ClientPID")) % known client not timed out
].
