-module(editor_tests).

-include_lib("eunit/include/eunit.hrl").

message_test() ->
  ?assert(0 =/= string:rstr(editor:createMessage(132), "lab18-1-3: message_number_132. Sent time:")).

calculateNewInterval_test() ->
  Random1 = editor:calculateNewInterval(0),
  Random2 = editor:calculateNewInterval(2000),
  Random3 = editor:calculateNewInterval(5000),
  Random4 = editor:calculateNewInterval(10000),
  ?assert(Random1 =:= 2000),
  ?assert((Random2 =:= 2000) or (Random2 =:= 3000)),
  ?assert((Random3 =:= 2500) or (Random3 =:= 7500)),
  ?assert((Random4 =:= 5000) or (Random4 =:= 15000)).
