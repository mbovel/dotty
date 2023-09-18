import language.experimental.setNotation


type Pos = {x: Int with x > 0}

inline val isPosTrue = "It Pos"
inline val isPosFalse = "It not Pos"

def isPos(x: Any) = x match
  case x: Pos => isPosTrue
  case _ => isPosFalse


type NonEmptyString = {s: String with !s.isEmpty}

type PoliteString = {s: NonEmptyString with s.head.isUpper && s.takeRight(6) == "please"}

inline val isPoliteTrue = "It Polite"
inline val isPoliteFalse = "It Impolite"

def isPolite(x: Any) = x match
  case x: PoliteString => isPoliteTrue
  case _ => isPoliteFalse

@main
def Test =

  val l0 = List[Int](-1, 0, 1, 4)
  val l1 = for case (x: Pos) <- l0 yield x
  val l2 = l0.collect{ case x: (Pos | 0) => math.sqrt(x) }
  assert( l1 == List(1, 4))
  assert( l2 == List(0, 1, 2))

  val examples = List( // val, isPosBool, isPoliteBool
    ("hello",                       false, false),
    ('c',                           false, false),
    (List(),                        false, false),

    (72,                            true,  false),
    (4,                             true,  false),
    (0,                             false, false),
    (-1,                            false, false),

    ("Give me the butter, please",  false, true ),
    ("give me the butter, please",  false, false),
    ("Give me the butter",          false, false),
    ("give me the butter",          false, false),
    ("",                            false, false), // if checks were not done in correct order, it would result in "".head which would fail at runtime
  )

  examples.foreach: (v, expectedPosBool, expectedPoliteBool) =>
    val expectedPosStr = if expectedPosBool then isPosTrue else isPosFalse
    assert( v.isInstanceOf[Pos] == expectedPosBool )
    assert( isPos(v) == expectedPosStr )

    val expectedPoliteStr = if expectedPoliteBool then isPoliteTrue else isPoliteFalse
    assert( v.isInstanceOf[PoliteString] == expectedPoliteBool )
    assert( isPolite(v) == expectedPoliteStr )

    println(s"$v: ${isPos(v)}, and ${isPolite(v)}")
