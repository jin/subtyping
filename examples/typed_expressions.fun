1
-1
1
a
true :: Bool
false :: Bool
{ a = 2, b = 3, c = 4 } :: { a: Int, b: Int, c: Int }
{ a = { d = 4 } :: { d : Int }, b = 3, c = 4 } :: { a : Int, b : Int, c : Int, d : Int }
(fn x :: (Int -> Int) => x)
(fn x :: (Int -> Int) => 2)
(fn x :: (Int -> Int) => true)
(fn x :: ({ a: Int } -> Int) => x) { a = 2 } :: { a: Int }
(fn x :: ({ a: Int } -> Int) => x.a) { a = 2 } :: { a : Int }
(fn x :: ({ a: Int } -> Int) => x.a + x.b) { a = 2, b = 3, c = 4 } :: { a : Int, b : Int }
(fn x :: ({ a: Int } -> { a: Int, b: Int }) => { a = x.a, b = 3 } :: { a: Int, b: Int }) { a = 2 } :: { a : Int }
