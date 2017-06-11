# IDL Compiler
Interface Definition Language Java Compiler for the middleware.

This application creates class and method stubs according to the IDL.

## IDL Example
```
module math_ops {
  class Calculator {
   double add(double a, double b);
   string getStr(double a);
 };
};
```
Would create a class Calculator within a math_ops module with the methods
`add(double a, double b)` and `getStr(double a)`.

For a more detailed description take a look in the documentation or the tests.
 
## Usage
To compile a <IDL-FILE-LOCATION>:
```bash
./bin/idl-compiler <IDL-FILE-LOCATION> <OUTPUT-FILE-LOCATION> 
```
This will output the generated Java Classes in the <OUTPUT-FILE-LOCATION>.

To build the compiler:
```bash
./gradlew distZip
```
This will output the zipped application in `./build/distributions/`
