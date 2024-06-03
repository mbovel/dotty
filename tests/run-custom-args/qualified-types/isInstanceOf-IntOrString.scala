def f[T] = ()

@main def Test: Unit =
  val n1: Any = 4
  if n1.isInstanceOf[Int | String] then println("n1 is an Int or a String")
  else println("n1 is not an Int or a String")

  val n2: Any = "Salut"
  if n2.isInstanceOf[Int | String] then println("n2 is an Int or a String")
  else println("n2 is not an Int or a String")

  val n3: Any = 3.1
  if n3.isInstanceOf[Int | String] then println("n3 is an Int or a String")
  else println("n3 is not an Int or a String")