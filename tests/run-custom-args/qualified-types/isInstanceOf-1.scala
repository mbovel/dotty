def f[T] = ()

def Test =
  val n1: Any = ???

  f[Int]

  if n1.isInstanceOf[Int] then println("n is an Int")
  else println("n is not an Int")

  if n1.isInstanceOf[{v: Int with v == 10}] then println("n is an Int with value 10")
  else println("n is not an Int with value 10")

class C1:
  def isInt =
    if isInstanceOf[Int] then println("I am an Int")
