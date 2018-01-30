# MiniLexer
A Lexer for the Minisculus language using Haskell's Alex


## Compile
This is written for Haskell's Lexer, Alex
First run through alex:
    alex Lexer.x

This generates a haskell source code file, which needs to be compiled:
    ghc Lexer.hs


to run individual test after compilation:
    $./Lexer <testFile>

=======
### Compile
This is written for Haskell's Lexer, Alex
First run through alex:
> alex Lexer.x

This generates a haskell source code file, which needs to be compiled:
> ghc Lexer.hs

### Execution
To run individual test after compilation:
> $./Lexer \<test-file\>
  
