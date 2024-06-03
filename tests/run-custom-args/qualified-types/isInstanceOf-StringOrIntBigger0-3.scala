def f[T] = ()

@main def Test: Unit =
  val n: Int = -10

  if n.isInstanceOf[ String | {v: Int with v > 0}] then println("n is an Int with value > 0 or a String")
  else println("n is not an Int with > 0 or a String")