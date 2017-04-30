# FunSub: A toy language with structural subtyping

The purpose of this toy language is to provide a concrete realisation of subtyping concepts.

The typechecker checks well-typedness of subtyping in records (depth, width, permutation) and functions (contravariant in arg, covariant in retval).

### Running

Assuming Haskell is installed and the user is in the project directory, running the following command will invoke the typechecker: 

```sh
runhaskell Main.hs examples/typed_expressions.fun
``` 

This produces an output similar to the following:

```
..
[Expression]: (fn x :: (Int -> Int) => 2)
[Typecheck][OK]: (Int -> Int)

[Expression]: (fn x :: (Int -> Int) => true)
[Typecheck][FAIL]: Type mismatch: expected Int, got Bool
..
```

### Architecture

The components of the language comprises of a REPL/reader, lexer, parser and typechecker. Some examples of FunSub expressions are stored in the `examples/` folder.

`Main.hs` is the entry point that takes in the filename for a file containing `FunSub` expressions. The lexer (`Lexer.hs`) defines reserved tokens and lexemes, and the parser (`Parser.hs`) uses `Parsec` on the tokens to generate an abstract syntax tree (AST) defined in `Syntax.hs`. Lastly, the AST is checked for well-typedness with rules defined in the typechecker (`Typecheck.hs`).

### Example {#sec:example}

The following is a valid expression in the `FunSub` syntax:

```fun
(fn x :: ({ a: Int, b: Int } -> { a: Int }) => x) 
  { a = 2, b = 2, c = true } :: { a: Int, b: Int, c: Bool }
```

In type systems without subtyping, this function application will not
typecheck because the record argument type does not match the parameter type, and the parameter type does not match the function body type even though it is an identity function. However in our subtyping
implementation, we are able to use concepts such as *variance* , *width* and *depth* record subtyping to make this expression well-typed.

### Implementation: Typechecker

The two main functions of the typechecker are `typecheck` and `isSubtype`. Their type signatures are defined as follows:

```hs
-- Typecheck.hs
newtype TypeEnv = TypeEnv (Map String Ty) 

isSubtype :: Ty -> Ty -> Bool
typecheck :: TypeEnv -> Expr -> Either String Ty
```

`isSubtype` takes in two types and recursively determines if the first
type is a subtype of the second.

`typecheck` takes in a type environment (a mapping of variables to
types) and an AST, and recursively determines if the expression is
well-typed. If it is ill-typed, an error message describing the issue is bubbled up and handled in `Main.hs`. If it is well-typed, the exact type is returned to the caller.
