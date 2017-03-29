-module(dlq_test).
-include_lib("eunit/include/eunit.hrl").

% Should create an empty DLQ
initDLQ_test() -> {[], 3, "Test"} = dlq:initDLQ(3, "Test").

% Should create an empty DLQ
delDLQ_empty_dlq_test() -> ok = dlq:delDLQ(dlq:initDLQ(3, "Test")).

expectedNr_empty_dlq_test() -> 1 = dlq:expectedNr(dlq:initDLQ(3, "Test")).
expectedNR_1item_dlq_test() -> 2 = dlq:expectedNr({[[1,"",1337,42]], 3, "Test"}).
expectedNR_with_one_message_dlq_test() -> 43 = dlq:expectedNr({[[42,"asd",1337,42]], 3, "Test"}).
