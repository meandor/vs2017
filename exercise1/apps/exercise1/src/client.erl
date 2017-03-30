-module(client).
-export([start/2]).


start(ServerAdress, 1) -> spawn_client(ServerAdress);
start(ServerAdress, Number) -> spawn_client(ServerAdress), start(ServerAdress, Number - 1).

spawn_client(ServerAddress) -> io:format("Spawne client...\n"), spawn(?MODULE, setup, [ServerAddress]).

loop(Server, Interval, NNr) -> ct:sleep(1000),  io:format("Sende nachricht...\n"), loop(Server, Interval, NNr + 1); % Send messages here

loop(Server, Interval, 5) ->io:format("Lese Nachricht...\n"), loop(Server, Interval, 0).
  % Read messages
