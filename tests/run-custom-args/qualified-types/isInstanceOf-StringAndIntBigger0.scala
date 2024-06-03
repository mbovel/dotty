import language.experimental.setNotation
def f[T] = ()

@main def Test: Unit =
  val n1: Any = "Salut"
  if n1.isInstanceOf[ String & Int & {v: Int with v > 0}] then println("n1 is an Int with value > 0 and a String")
  else println("n1 is not an Int with > 0 and a String")

  val n2: Any = 10
  if n2.isInstanceOf[ String & {v: Int with v > 0}] then println("n2 is an Int with value > 0 and a String")
  else println("n2 is not an Int with > 0 and a String")

  val n3: Any = -10
  if n3.isInstanceOf[ String & {v: Int with v > 0}] then println("n3 is an Int with value > 0 and a String")
  else println("n3 is not an Int with > 0 and a String")