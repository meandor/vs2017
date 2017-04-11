# Message of the Day

This Erlang OTP App implements a client/server architecture for a message of the day app.

Clients send messages to a server. The server keeps the messages in a certain order
 and sends them out to its clients in their respective order.
 
If the clients are already known to the server and already received messages
 only get messages that they didn't receive yet.

## Build
To compile everything:
````bash
erl -make
````

## Run
To start with compiled files:
````bash
erl -pa ebin/
````

To start with compiled files and given name@host:
````bash
erl -pa ebin/ -sname name@host
````

## Usage
To reload everything in the erlang shell:
````erlang
make:all([load]).
````

To start the server:
````erlang
server:startMe().
````

To start multiple clients:
````erlang
client:startClients().
````
This will start a scenario which will launch multiple clients (configured in configs) that send messages to the server

To start one client:
````erlang
client:spawnClient(ServerPID).
````

## Test
To test a <MODULENAME> in the erl shell:
````erlang
eunit:test(<MODULENAME>).
````

For all tests:
````erlang
eunit:test(cmem), eunit:test(dlq), eunit:test(server), eunit:test(reader), eunit:test(hbq), eunit:test(client), eunit:test(editor).
````

## Configuration
All configurable variables are located in `./config`.
The server and the client can both be configured. Each configuration is found in the respective `*.cfg` file.