-module(hbq_tests).
-include_lib("eunit/include/eunit.hrl").

% Should create an empty DLQ
initHBQ_test() -> [[], 3] = hbq:initHBQ(3, 'test.log').

