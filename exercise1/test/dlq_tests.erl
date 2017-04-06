-module(dlq_tests).

-include_lib("eunit/include/eunit.hrl").

-export([testClientExpectingMessage/2]).

emptyDLQSize3() -> [[], 3].
twoElementsDLQSize2() -> [[[2, "foobar42", 42, 13, 123], [1, "foobar1", 22, 14, 134]], 2].

testClientExpectingMessage(Message, Terminated) ->
  receive
    {reply, ReceivedMessage, Terminated} ->
      MessageWithoutTS = lists:droplast(ReceivedMessage),
      if
        Message =:= MessageWithoutTS ->
          ok;
        true ->
          testClientExpectingMessage(Message, Terminated)
      end
  end.

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

deliverMSG_existent_message_not_terminated_test() ->
  ClientPID = spawn(?MODULE, testClientExpectingMessage, [[1, "foobar1", 22, 14, 134], false]),
  dlq:deliverMSG(1, ClientPID, twoElementsDLQSize2(), 'test.log'),
  timer:sleep(1000),
  ?assert(undefined =:= erlang:process_info(ClientPID)).

deliverMSG_non_existent_message_terminated_test() ->
  ClientPID = spawn(?MODULE, testClientExpectingMessage, [[-1, "No new messages", -1, -1, -1], true]),
  dlq:deliverMSG(3, ClientPID, twoElementsDLQSize2(), 'test.log'),
  timer:sleep(1000),
  ?assert(undefined =:= erlang:process_info(ClientPID)).

deliverMSG_empty_DLQ_terminated_test() ->
  ClientPID = spawn(?MODULE, testClientExpectingMessage, [[-1, "No new messages", -1, -1, -1], true]),
  dlq:deliverMSG(3, ClientPID, emptyDLQSize3(), 'test.log'),
  timer:sleep(1000),
  undefined = erlang:process_info(ClientPID).

deliverMSG_most_up_to_date_message_terminated_test() ->
  ClientPID = spawn(?MODULE, testClientExpectingMessage, [[2, "foobar42", 42, 13, 123], true]),
  dlq:deliverMSG(2, ClientPID, twoElementsDLQSize2(), 'test.log'),
  timer:sleep(1000),
  ?assert(undefined =:= erlang:process_info(ClientPID)).

deliverMSG_message_inbetween_not_terminated_test() ->
  ClientPID = spawn(?MODULE, testClientExpectingMessage, [[2, "foobar42", 42, 13, 123], false]),
  dlq:deliverMSG(2, ClientPID, [[[3, "foobar41", 4, 113, 1423], [2, "foobar42", 42, 13, 123], [1, "foobar1", 22, 14, 134]], 4], 'test.log'),
  timer:sleep(1000),
  ?assert(undefined =:= erlang:process_info(ClientPID)).
