import language.experimental.setNotation
def f[T] = ()

@main def Test: Unit =
  val n1: Any = "salut"
  if n1.isInstanceOf[{v: String with v != ""}] then println("n1 is not empty")
  else println("n1 is empty")

  val n2: Any = ""
  if n2.isInstanceOf[{v: String with v != ""}] then println("n2 is not empty")
  else println("n2 is empty")

  val n3: Any = 4
  if n3.isInstanceOf[{v: String with v != ""}] then println("n3 is not empty")
  else println("n3 is empty")

  val n4: String = "salut"
  if n4.isInstanceOf[{v: String with v != ""}] then println("n4 is not empty")
  else println("n4 is empty")

  val n5: String = ""
  if n5.isInstanceOf[{v: String with v != ""}] then println("n5 is not empty")
  else println("n5 is empty")
