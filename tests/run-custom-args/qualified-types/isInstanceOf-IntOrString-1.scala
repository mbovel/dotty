def f[T] = ()

@main def Test: Unit =
  val n = 4

  if n.isInstanceOf[Int | String] then println("n is an Int or a String")
  else println("n is not an Int or a String")