def f[T] = ()

@main def Test: Unit =
  val n: String = ???

  if n.isInstanceOf[ String & {v: Int with v > 0}] then println("n is an Int with value > 0 and a String")
  else println("n is not an Int with > 0 and a String")