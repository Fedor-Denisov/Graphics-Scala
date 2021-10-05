var x: Int = 6
val sq = x * x
def sqrt = x * x
sq
sqrt
x = 9
sq
sqrt

x.isInstanceOf[Any]

val foo: (Int, Int) => Int = _ * 2 + _ * 3
foo(2, 3)

val res: (Int, Int) => Int = foo(_, _) * 4
res(2, 3)

print(-100f/ -400)
