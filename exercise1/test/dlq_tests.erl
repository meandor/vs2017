-module(dlq_tests).
-include_lib("eunit/include/eunit.hrl").

emptyDLQSize3() -> [[], 3].
twoElementsDLQSize2() -> [[[2, "foobar42", 42, 13, 123], [1, "foobar1", 22, 14, 134]], 2].

% Should create an empty DLQ
initDLQ_test() -> ?_assert(emptyDLQSize3() =:= dlq:initDLQ(3, 'test.log')).

% Should delete a DLQ
delDLQ_empty_dlq_test() -> ?_assert(ok =:= dlq:delDLQ(emptyDLQSize3())).

% Should return the next expected message number
expectedNr_test_() ->
  [
    ?_assert(1 =:= dlq:expectedNr(emptyDLQSize3())), % empty dlq
    ?_assert(2 =:= dlq:expectedNr([[[1, "", 1337, 42, 12]], 3])), % 1 item in the dlq
    ?_assert(3 =:= dlq:expectedNr(twoElementsDLQSize2())) % 1 item in the dlq
  ].

push2DLQ_valid_insertion_in_empty_dlq_test() ->
  [[[1, "foobar", 1337, 42, Timestamp]], 3] = dlq:push2DLQ([1, "foobar", 1337, 42], emptyDLQSize3(), 'test.log'),
  Timestamp > 0.
push2DLQ_valid_insertion_in_full_dlq_test() ->
  [[[3, "foobar", 1337, 42, Timestamp], [2, "foobar42", 42, 13, _]], 2] = dlq:push2DLQ([3, "foobar", 1337, 42], twoElementsDLQSize2(), 'test.log'),
  Timestamp > 0.

deliverMSG_existent_message_test() ->
  1 = dlq:deliverMSG(1, 3, twoElementsDLQSize2(), 'test.log').
