1
-1
1
a
true
false
{ a = 2, b = 3, c = 4 } :: { a: Int, b: Int, c: Int }
{ a = { d = 4 } :: { d : Int }, b = 3, c = 4 } :: { a : { d : Int }, b : Int, c : Int }
{ a = { d = 4 } :: { d : Int }, b = 3, c = 4 } :: { a : { d : Int } }
{ a = { d = 4 } :: { d : Int }, b = 3, c = 4 } :: { a : { c : Int } }
(fn x :: (Int -> Int) => x)
(fn x :: (Int -> Int) => y)
(fn x :: (Int -> Int) => 2)
(fn x :: (Int -> Int) => true)
(fn x :: ({ a: Bool } -> Int) => x.a)
(fn x :: ({ a: Int } -> Int) => x.b)
(fn x :: ({ a: Int } -> Int) => x) { a = 2 } :: { a: Int }
(fn x :: ({ a: Int, b: Int } -> { a: Int }) => x) { a = 2, b = 2, c = true } :: { a: Int, b: Int, c: Bool }
(fn x :: ({ a: Int } -> Int) => x.a) { a = 2 } :: { a : Int }
(fn x :: ({ a: Int } -> Bool) => x.a) { a = 2 } :: { a : Int }
(fn x :: ({ a: Int } -> { a: Int, b: Int }) => { a = x.a, b = 3 } :: { a: Int, b: Int }) { a = 2 } :: { a : Int }
(fn x :: ({ a: { c : Int} } -> Int) => x.a.c) { a = { c = 4 } :: { c : Int }, b = 3 } :: { a: { c : Int }, b: Int }
(fn x :: ({ a: Int, b: Int } -> Int) => x.b) { a = 2 } :: { a: Int }
(fn x :: ({ a: { b: Int, c: Int } } -> Int) => x.a.b) { a = { b = 2, c = 3 } :: { b: Int, c : Int } } :: { a: { b: Int, c: Int } }
(fn x :: ({ a: { b: Int } } -> Int) => x.a.b) { a = { b = 2, c = 3 } :: { b: Int, c : Int } } :: { a: { b: Int, c: Int } }
(fn x :: ({ a: { b: Int, c: Int } } -> Int) => x.a.b) { a = { c = 3 } :: { c : Int } } :: { a: { c: Int } }
(fn x :: ({ a: { b: Int, c: Int } } -> { a: Int } ) => { a = x.a.c, b = x.a.b } :: { a: Int, b: Int }) { a = { b = 3, c = 3 } :: { b: Int, c: Int } } :: { a: { b: Int, c: Int } }
