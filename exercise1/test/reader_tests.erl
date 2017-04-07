-module(reader_tests).

-include_lib("eunit/include/eunit.hrl").

-export([test_server_with_one_reply/2, test_server_with_multiple_replies/1]).

is_empty_list([]) -> true;
is_empty_list(_) -> false.

test_server_with_one_reply(Message, Terminated) ->
  receive
    {ClientPID, getmessages} ->
      ClientPID ! {reply, Message, Terminated}
  end.

test_server_with_multiple_replies([]) -> ok;

test_server_with_multiple_replies([Message | Rest]) ->
  receive
    {ClientPID, getmessages} ->
      ClientPID ! {reply, Message, is_empty_list(Rest)},
      test_server_with_multiple_replies(Rest)
  end.

getmessages_with_dummy_message_reply_test() ->
  ServerPID = spawn(?MODULE, test_server_with_one_reply, [[-1, "No new messages", -1, -1, -1, -1], true]),
  ClientPID = spawn(reader, start_reading, [false, 'test.log', [1, 3, 4], ServerPID]),
  timer:sleep(100),
  ?assert(undefined =:= erlang:process_info(ServerPID)),
  ?assert(undefined =:= erlang:process_info(ClientPID)).

getmessages_with_two_messages_reply_test() ->
  ServerPID = spawn(?MODULE, test_server_with_multiple_replies, [[[1, "foobar1", 22, 14, 134, 1], [2, "foobar42", 42, 13, 123, 1]]]),
  ClientPID = spawn(reader, start_reading, [false, 'test.log', [1, 3, 4], ServerPID]),
  ?assert(undefined =/= erlang:process_info(ServerPID)),
  ?assert(undefined =/= erlang:process_info(ClientPID)),
  timer:sleep(100),
  ?assert(undefined =:= erlang:process_info(ServerPID)),
  ?assert(undefined =:= erlang:process_info(ClientPID)).