1 :: Int
-1 :: Int
1 :: Int 
a :: Int
true :: Bool
false :: Bool
{ a = 2 :: Int, b = 3 :: Int, c = 4 :: Int } :: { a: Int, b: Int, c: Int }
{ a = { d = 4 :: Int }, b = 3 :: Int, c = 4 :: Int }
(fn x => x) { a = 2 :: Int }
(fn x => { a = x, b = 3 :: Int }) { a = 2 :: Int }
(fn x => x.a) { a = 2 :: Int }
(fn x => x.a) { a = 2 :: Int, b = 3 :: Int }
