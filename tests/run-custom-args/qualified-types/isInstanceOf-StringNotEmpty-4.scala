def f[T] = ()

@main def Test: Unit =
  val n: String = ???

  if n.isInstanceOf[ {v: String with v != ""}] then println("n is not empty")
  else println("n is empty")