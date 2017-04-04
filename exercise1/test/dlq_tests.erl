-module(dlq_tests).
-include_lib("eunit/include/eunit.hrl").

% Should create an empty DLQ
initDLQ_test() -> ?_assert([[], 3] =:= dlq:initDLQ(3, 'test.log')).

% Should delete a DLQ
delDLQ_empty_dlq_test() -> ?_assert(ok =:= dlq:delDLQ([[], 3])).

% Should return the next expected message number
expectedNr_test_() ->
  [
    ?_assert(1 =:= dlq:expectedNr(dlq:initDLQ(3, 'test.log'))), % empty dlq
    ?_assert(2 =:= dlq:expectedNr([[[1, "", 1337, 42]], 3])), % 1 item in the dlq
    ?_assert(43 =:= dlq:expectedNr([[[42, "asd", 1337, 42]], 3])) % 1 item in the dlq
  ].

push2DLQ_valid_insertion_in_empty_dlq_test() ->
  [[[1, "foobar", 1337, 42, Timestamp]], 5] = dlq:push2DLQ([1, "foobar", 1337, 42], dlq:initDLQ(5, 'test.log'), 'test.log'),
  Timestamp > 0.
push2DLQ_valid_insertion_in_full_dlq_test() ->
  DLQ = [[[2, "foobar42", 42, 13, 123], [1, "foobar1", 22, 14, 134]], 2],
  [[[3, "foobar", 1337, 42, Timestamp], [2, "foobar42", 42, 13, _]], 2] = dlq:push2DLQ([3, "foobar", 1337, 42], DLQ, 'test.log'),
  Timestamp > 0.

deliverMSG_existent_message_test() ->
  DLQ = [[[3, "foobar", 1337, 42, 123], [2, "foobar42", 42, 13, 321]], 2],
  2 = dlq:deliverMSG(2, 3, DLQ, 'test.log').
