-module(dlq_test).
-include_lib("eunit/include/eunit.hrl").

% Should create an empty DLQ
initDLQ_test() -> {[], 3, "Test"} = dlq:initDLQ(3, "Test").

% Should create an empty DLQ
delDLQ_empty_dlq_test() -> ok = dlq:delDLQ(dlq:initDLQ(3, "Test")).