# Turtle Compiler
### A Haskell project by Andrew M. Hall

#### Description
I wrote this project in my second year at ANU, in excited preperation for taking Comp3610, for which this is the second assignment (See Turtle/docs/Asst2.pdf for details). I've chosen Haskell because of the safety guarantees it provides, and the fact that the Alex / Happy modules provide a great modern variant of flex / bison.

This compiler has several extensions on the original specification, including a full Syntax Check and dynamic variable scoping. A Debug mode prints out binaries in a readable instruction format. There is a 2 stage compilation allowing for functions to be used before they're declared (as in any modern language).

The compiler uses arrays (instead of haskell lists) to allow constant time random access and efficient compilation.

##### Analyse.hs
Implements a static analysis compilation phase as well as building up a symbol table which is passed into the compiler to remove the need for function forward declarations. The semantic checks are enumerate in `ErrorType`.

##### Lexer.x
An Alex specification file, defined the tokens that make up a Turtle program

##### Parser.y
A Happy specification file, defines the syntax tree of the program and how to parse it.

##### Turtle.hs
Provides the interface to deal with, and manipulate turtle code. This includes the `Instruction` type, and the `Vector` manipulation. Type polymorphism is used to allow raw and debugging representations of Memory.

##### Compile.hs
Compile the output from Analyse.hs into a vector of instructions. This includes a back patching stage.

### Building
The program can be built using Haskell Stack. Simply enter the directory and execute.

```
stack build
```

### Testing

There are a series of test programs in the `Turtle/` directory, to run the compiler over all the test programs, run the `Turtle/test.sh` script.

The Compiler can be invoked like so

```
stack exec TurtleCompiler -- $(FLAGS) < $(INPUT)
```

where `$(FLAGS)` can be `--help` or `--debug`.
