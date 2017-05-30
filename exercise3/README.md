# exercise3

A `clojure` app receiving and sending messages via multicast sockets.
 
This microservice represents a sending and receiving station on one medium (socket).
The network traffic is organized on that medium by STDMA.

## Usage
To run the tests:
````bash
./lein.sh test
````

To build the jar
````bash
./lein.sh uberjar
````

To run the app
````bash
./lein.sh run [args]
````

To run the jar:
````bash
java -jar exercise3-0.1.0-standalone.jar [args]
````

Where args are:

`--help`, or

`<interfaceName> <mcastAddress> <receivePort> <stationClass> <UTCoffsetMs>`

To run the scenario:
````bash
./bin/startStations.sh <interfaceName> <mcastAddress> <receivePort> <firstIndex> <lastIndex> <stationClass> <UTCoffsetMs>
````
