1
-1
{ a = 2, b = 3, c = 4 }
{ a = { d = 4 }, b = 3, c = 4 }
(fn x => x) { a = 2 }
(fn x => { a = x, b = 3 }) { a = 2 }
(fn x => x.a) { a = 2 }
(fn x => x .a) { a = 2, b = 3 }
