import compiletime.constValueTuple

def test(tup: ("one", "two", "three")) = tup.toList

def test2 =
  val ll0: Tuple3["one", "two", "three"] = constValueTuple[("one", "two", "three")]
  val ll1 = constValueTuple[("one", "two", "three")].toList
  val ll3: List["one" | ("two" | ("three" | Nothing))] = constValueTuple[("one", "two", "three")].toList
  val ll4: List["one" | ("two" | "three")] = constValueTuple[("one", "two", "three")].toList

  inline def labels[Labels <: Tuple](using ev: Tuple.Union[Labels] <:< String): List[String] =
    val tmp = constValueTuple[Labels].toList
    ev.substituteCo( tmp )

  val strings = labels[("one", "two", "three")]
  // returns List(one, two, three)
  println(strings.mkString("<", ", ", ">"))

type F2[X, Y]
type F[X] = F2[X, X]
def toF(x: Any): F[x.type] = ???
def test3[T] = toF((???): T)
