1
-1
true
false
1 + 2
3 * 4
1 + 2 * 3 + 4
"abc"
"d"
""
2 == -3
4 <= 3
5 > 3
if 2 <= 3 then 4 else 5
if 2 == 3 then if 2 <= 3 then 5 else if true then 42 else -32 else 5
fun f x => true 
fn y => fn z => y 
fun f x => x + 3
fun f x => fn y => y
let g = 2 in 3 + 5
let g = fn y => y in g (3 * 5)
let g = fun h x => x (fn y => y) in g (fn z => z)
{}
{ }
{ x = 2 }
{ y = 2,z = true }
{ y = 2, z = true, foo = 3 * 4 }
{ y = 2, z = { a = 3, b = 3, c = { d = false } } }
