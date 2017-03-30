-module(reader_client).
-export([read/3, calculateNewInterval/1]).

read(Server, Interval, 5) ->
  Server ! {getmessages, self()},
  logger ! {debug, "Getting a message."},
  receive
    {Msg, false} ->
      logger ! {debug, lists:concat(["Got message: ", Msg])},
      read(Server, Interval, 5);
    {Msg, true} ->
      logger ! {debug, lists:concat(["Got message: ", Msg])},
      read(Server, Interval, 0);
    _X ->
      logger ! {debug, "Unknown error."}
  end.

calculateNewInterval(Interval) ->
  Increase = werkzeug:bool_rand(),
  Value = trunc(max(Interval * 0.5, 1000)),
  if
    Increase ->
      Total = Interval + Value,
      logger ! {debug, lists:concat(["Setting ", integer_to_list(Total), " as new interval."])},
      Total;
    true ->
      Total = max(Interval - Value, 1000),
      logger ! {debug, lists:concat(["Setting ", integer_to_list(Total), " as new interval."])},
      Total
  end.

