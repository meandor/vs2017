-module(dlq_test).
-include_lib("eunit/include/eunit.hrl").

% Should create an empty DLQ
initDLQ_test() -> {[], 3, "Test"} = dlq:initDLQ(3, "Test").