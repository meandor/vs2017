# Enchiridion
> Brave hero, let your sword be sheathed, for though it be your best and most trusty tool, it cannot help you when
> traversing the treacherous terrain upon which you now embark.
>
> ~ Enchiridion Chapter four (Adventure Time)

Java Application used to Start a NameService Server.
Middleware Programs can register here with a specific protocol.

For more information please consider the documentation.
 
## Usage
To start the NameService on a <PORT>:
```bash
./bin/enchriridion <PORT>
```

To build the nameservice:
```bash
./gradlew distZip
```
This will output the zipped application in `./build/distributions/`

To install it to your local maven repository:
```bash
./gradlew install
```