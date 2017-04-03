-module(client).
-export([start/2]).

start(ServerAddress, Number) -> logger:create(lists:concat([node(), ".log"])), start_n(ServerAddress, Number).

start_n(ServerAddress, 1) -> spawn_client(ServerAddress);
start_n(ServerAddress, Number) -> spawn_client(ServerAddress), start_n(ServerAddress, Number - 1).

spawn_client(ServerAddress) -> io:format("Spawne client...\n"), spawn(reader_client, setup, [ServerAddress]).

