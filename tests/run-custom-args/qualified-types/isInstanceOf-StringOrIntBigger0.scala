import language.experimental.setNotation
def f[T] = ()

@main def Test: Unit =
  val n1: Any = ""
  if n1.isInstanceOf[ String | {v: Int with v > 0}] then println("n1 is an Int with value > 0 or a String")
  else println("n1 is not an Int with > 0 or a String")

  val n2: Any = 10
  if n2.isInstanceOf[ String | {v: Int with v > 0}] then println("n2 is an Int with value > 0 or a String")
  else println("n2 is not an Int with > 0 or a String")

  val n3: Any = -10
  if n3.isInstanceOf[ String | {v: Int with v > 0}] then println("n3 is an Int with value > 0 or a String")
  else println("n3 is not an Int with > 0 or a String")

  val n4: Any = 1.5
  if n4.isInstanceOf[ String | {v: Int with v > 0}] then println("n4 is an Int with value > 0 or a String")
  else println("n4 is not an Int with > 0 or a String")