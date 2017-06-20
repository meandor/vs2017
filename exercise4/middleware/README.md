# Candy Kingdom
> In the Land of Ooo is a Candy Kingdom <br>
> Whose sidewalks you can eat<br>
> And everyone who lives in there<br> 
> Is made of something sweet<br>
> There are many candy people<br>
> Far too many for to name <br>
> But if you care to take a dare<br> 
> We'll try it just the same<br>
> 
> ~ Candy Kingdom Song (Adventure Time)

Java Library providing a CORBA inspired middleware for remote procedure calls.

## Usage

To use the middleware you have to
1. Create Java Class Stubs from IDL-files (Interface Definition Language) with the shipped [compiler (neptr)](https://github.com/meandor/vs2017/tree/master/exercise4/idl-compiler) 
2. Modify the generated classes and add business logic to them (only server-side)
3. Build a client with the generated client stub classes
4. Run a NameService Server ([enchiridion](https://github.com/meandor/vs2017/tree/master/exercise4/nameservice))
5. Use the mware_lib ObjectBroker in the client to call methods on the remote server object

For example in the client code:
```java
ObjectBroker objectBroker = ObjectBroker.init(host, port, true);
NameService nameService = objectBroker.getNameService();
Object rawRef = nameService.resolve("foobar");
 _CalculatorProxy proxy = _CalculatorImplBase.narrowCast(rawRef);
proxy.doStuff();

```

For a more detailed example take a look at: [gunter](https://github.com/meandor/gunter)

## Building
To build:
```bash
./gradlew jar
```
Will build the jar in ./build/libs/

## Testing
To test:
```bash
./gradlew check
```

## Releasing locally
To release locally in local maven repository:
```bash
./gradlew install
```

You can then use it in your gradle `build.gradle` in dependencies section:
`compile('de.haw.vs:candy-kingdom:$VERSION')`

Where `$VERSION` is the version in `gradle.properties`.

For more info take a look at the documentation.