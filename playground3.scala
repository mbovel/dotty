def foo(a: Int): {v: Int with v > a} = ???


def main =
  val x: Int = ???
  val y = foo(42)
  val y2 = foo(??? : Int)
  val y3 = foo(x)

  y2 == (y3 == y)

  (y2 == y3) == y
