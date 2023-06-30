import language.experimental.setNotation

val b = true
type Pos = {x: Int with x > 0}
val scrut: Any = 4

@main
def Test =
  println(isPos(4))
  println(isPos(0))
  println(isPos(-1))

  println(isPos("hello"))
  println(isPos('c'))
  println(isPos(List()))

def isPos(x: Any) = x match
  case x: Pos => "It Pos"
  case _ => "It not Pos"

