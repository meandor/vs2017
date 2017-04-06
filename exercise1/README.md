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

To start with compiled files:
````bash
erl -pa ebin/
````

To load everything in the erl shell:
````erlang
make:all([load]).
````
[make:all([load]).
## Test
To test a $MODULENAME in the erl shell:
````erlang
eunit:test($MODULENAME).
````