# CPSC 521: Assignment 2
This is the lambda lifting assignment.

# Directory overview
```
lambda-lift.pdf
```
This is the file which describes the assignment. You should read this!

```
AlphaRename.hs, CallGraph.hs, LambdaLift.hs, A2.hs
```
Files you should fill out to do the following steps respectively:
    - Alpha rename a program
    - Generate the call graph of a program
    - Lambda lift the program
    - Combine each of the steps to go from a program, to the lambda lifted
      program

```
README.md  
```
This (hopefully) helpful file.

```
AST.hs  
```
The AST of the tree for which you should lambda lift

```
ASTParse.hs  
```
Parsing utilities provided for convenience to help you write your own test cases.

```
ExamplePrograms.hs  
```
Some example programs as raw string literals to play with.

```
Lib/Monads.hs
Lib/Parser.hs
Lib/RawString.hs
```
These are generalized library files which do the following respectively.
    - ``Monads.hs`` includes many common monads in Haskell (including the
      StateT monad) which may help you with your assignment.
    - ``Parser.hs`` includes some utilities used to write the parser (you do
      not need to be concerned about this)
    - ``RawString.hs`` includes a ``QuasiQuote`` to write raw string literals
      in Haskell (this is helpful for writing test programs, although not all
      Haskell compilers will support this)


# Tips
PLEASE READ ``./lambda-lift.pdf``.

# Playing with your work in ghci.
Since ``A2.hs`` imports all the files, it's probably easiest to open ``ghci`` by typing
```
ghci A2
```
so that you can play with each function in each file to see the outputs (given
whichever input you have)

## Using the parser
If you have a program string say
```
assign0RawStringLiteral= [r|
fun main(x, y) =
    let fun add(p) = add_to_x(p)
        fun add_to_x(q) = add_to_y(q) + x
        fun add_to_y(q) = q + y
    in add(y + x) end
|]
```
(where we use `[r| .. |]` for raw string literals -- see `./ExamplePrograms.hs`).
So, type the following in ghci
```
unsafeParseProg assign0RawStringLiteral
```
to parse the program (this returns something of type ``Prog String String``).
If the parse fails, ``unsafeParseProg`` will throw an error.

# GradeScope
Please submit ONLY the following files
    `A2.hs`
    `CallGraph.hs`
    `AlphaRename.hs`
    `LambdaLift.hs`

The only function that will be autograded is `A2.runLambdaLift`. 
