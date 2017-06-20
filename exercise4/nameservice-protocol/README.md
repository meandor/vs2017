# NameService Protocol

Protocol definitions for the NameService (Enchriridion).

If you want to know how it works in detail, please have a look in the documentation.

## Usage
### Generate jar
To generate a jar:
```bash
./gradlew jar
```
which will create a jar in `./build/libs`

### Release new version
To release a new version do:
```bash
./gradlew release
```

and follow the instructions

### Publish to local maven repository
To use it locally you have to install it
```bash
./gradlew install
```

After that it is in your local maven repository and can be used in your projects locally
if you include the `mavenLocal()` repository in your `build.gradle`.