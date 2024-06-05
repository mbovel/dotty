import language.experimental.setNotation
def f[T] = ()

@main def Test: Unit =
  val n1 = 10
  if n1.isInstanceOf[({v: Int with v > 0})] then println("n1 is an Int with value > 0")
  else println("n1 is not an Int with > 0")

  val n2 = 0
  if n2.isInstanceOf[({v: Int with v > 0})] then println("n2 is an Int with value > 0")
  else println("n2 is not an Int with > 0")

  val n3 = -1
  if n3.isInstanceOf[({v: Int with v > 0})] then println("n3 is an Int with value > 0")
  else println("n3 is not an Int with > 0")

