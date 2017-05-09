# GGT App

This Erlang OTP App implements a distributed algorithm for the largest common divisor.

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

Our modules and processes are dependent on a name service.
To start one simply start it in your node:
````erlang
nameservice:start().
````
Make sure to update the config files with the nameservice node name.

To start a starter:
````erlang
starter:start(<StarterID>).
````

To start a coordinator:
````erlang
koordinator:start().
````

For further usage please have a look at the sequence diagram in the documentation.

## Test
To test a <MODULENAME> in the erl shell:
````erlang
eunit:test(<MODULENAME>).
````

For all tests:
````erlang
eunit:test(koordinator),eunit:test(starter),eunit:test(ggt).
````

## Configuration
All configurable variables are located in `./config`.
The server and the client can both be configured. Each configuration is found in the respective `*.cfg` file.

The test configurations for the modules are located in the `./test-config` folder.