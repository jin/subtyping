# Depth & Width subtyping

This is an implementation of depth and width subtyping in Haskell.

## Example

Running:

```sh
runhaskell Main.hs examples/typed_expressions.fun
```

Output:
```haskell
[Expression]: 1
[Typecheck] [OK]: Int

[Expression]: -1
[Typecheck] [OK]: Int

[Expression]: 1
[Typecheck] [OK]: Int

[Expression]: a
[Typecheck] [FAIL]: Variable a is not defined

[Expression]: true
[Typecheck] [OK]: Bool

[Expression]: false
[Typecheck] [OK]: Bool

[Expression]: { a = 2, b = 3, c = 4 } :: { a: Int, b: Int, c: Int }
[Typecheck] [OK]: { a: Int, b: Int, c: Int }

[Expression]: { a = { d = 4 } :: { d : Int }, b = 3, c = 4 } :: { a : { d : Int }, b : Int, c : Int }
[Typecheck] [OK]: { a: { d: Int }, b: Int, c: Int }

[Expression]: { a = { d = 4 } :: { d : Int }, b = 3, c = 4 } :: { a : { d : Int } }
[Typecheck] [OK]: { a: { d: Int } }

[Expression]: { a = { d = 4 } :: { d : Int }, b = 3, c = 4 } :: { a : { c : Int } }
[Typecheck] [FAIL]: Incorrect record type

[Expression]: (fn x :: (Int -> Int) => x)
[Typecheck] [OK]: Int -> Int

[Expression]: (fn x :: (Int -> Int) => y)
[Typecheck] [FAIL]: Variable y is not defined

[Expression]: (fn x :: (Int -> Int) => 2)
[Typecheck] [OK]: Int -> Int

[Expression]: (fn x :: (Int -> Int) => true)
[Typecheck] [FAIL]: Type mismatch: expected Int, got Bool

[Expression]: (fn x :: ({ a: Bool } -> Int) => x.a)
[Typecheck] [FAIL]: Type mismatch: expected Int, got Bool

[Expression]: (fn x :: ({ a: Int } -> Int) => x.b)
[Typecheck] [FAIL]: Unable to lookup field b in { a: Int }

[Expression]: (fn x :: ({ a: Int } -> Int) => x) { a = 2 } :: { a: Int }
[Typecheck] [FAIL]: Type mismatch: expected Int, got { a: Int }

[Expression]: (fn x :: ({ a: Int, b: Int } -> { a: Int }) => x) { a = 2, b = 2, c = true } :: { a: Int, b: Int, c: Bool }
[Typecheck] [OK]: { a: Int }

[Expression]: (fn x :: ({ a: Int } -> Int) => x.a) { a = 2 } :: { a : Int }
[Typecheck] [OK]: Int

[Expression]: (fn x :: ({ a: Int } -> Bool) => x.a) { a = 2 } :: { a : Int }
[Typecheck] [FAIL]: Type mismatch: expected Bool, got Int

[Expression]: (fn x :: ({ a: Int } -> { a: Int, b: Int }) => { a = x.a, b = 3 } :: { a: Int, b: Int }) { a = 2 } :: { a : Int }
[Typecheck] [OK]: { a: Int, b: Int }

[Expression]: (fn x :: ({ a: { c : Int} } -> Int) => x.a.c) { a = { c = 4 } :: { c : Int }, b = 3 } :: { a: { c : Int }, b: Int }
[Typecheck] [OK]: Int

[Expression]: (fn x :: ({ a: Int, b: Int } -> Int) => x.b) { a = 2 } :: { a: Int }
[Typecheck] [FAIL]: Invalid argument of type { a: Int } is not a subtype of the parameter type { a: Int, b: Int }

[Expression]: (fn x :: ({ a: { b: Int, c: Int } } -> Int) => x.a.b) { a = { b = 2, c = 3 } :: { b: Int, c : Int } } :: { a: { b: Int, c: Int } }
[Typecheck] [OK]: Int

[Expression]: (fn x :: ({ a: { b: Int } } -> Int) => x.a.b) { a = { b = 2, c = 3 } :: { b: Int, c : Int } } :: { a: { b: Int, c: Int } }
[Typecheck] [OK]: Int

[Expression]: (fn x :: ({ a: { b: Int, c: Int } } -> Int) => x.a.b) { a = { c = 3 } :: { c : Int } } :: { a: { c: Int } }
[Typecheck] [FAIL]: Invalid argument of type { a: { c: Int } } is not a subtype of the parameter type { a: { b: Int, c: Int } }```

## Width subtyping

## Depth subtyping
