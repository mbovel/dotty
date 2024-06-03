def f[T] = ()

@main def Test: Unit =
  val n = -1

  if n.isInstanceOf[({v: Int with v > 0})] then println("n is an Int with value > 0")
  else println("n is not an Int with > 0")