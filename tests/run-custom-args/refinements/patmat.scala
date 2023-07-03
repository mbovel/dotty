import language.experimental.setNotation

val b = true

type NonEmptyString = {s: String with !s.isEmpty}

type PoliteString = {s: NonEmptyString with s.head.isUpper && s.takeRight(6) == "please"}


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

  println(isPolite("Give me the butter, please"))
  println(isPolite("give me the butter, please"))
  println(isPolite("Give me the butter"))
  println(isPolite("Give me the butter"))
  println(isPolite("")) // if checks were not done in correct order, it would result in "".head which would fail at runtime
  println(isPolite(72))

def isPos(x: Any) = x match
  case x: Pos => "It Pos"
  case _ => "It not Pos"

def isPos2(x: Int) = x match
  case x: Pos => "It Pos"
  case _ => "It not Pos"


def isPolite(x: Any) = x match
  case x: PoliteString => "It Polite"
  case _ => "It Impolite"
