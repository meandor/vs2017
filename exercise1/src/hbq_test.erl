-include_lib("eunit/include/eunit.hrl").
-module(hbq_test).

% Should create an empty DLQ
initHBQ_test() -> [[], 3] = hbq:initHBQ(3, 'test.log').

