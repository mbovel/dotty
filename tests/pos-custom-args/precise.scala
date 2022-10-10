
dependent case class Foo(v: Int)

dependent def precise() =
  val v1 = 1
  val v2 = 2 + v1
  dependent def isString(x: Any) = x match
    case _: String => true
    case _ => false
  val v3 = isString(42)
  val v4 = Foo(42)
