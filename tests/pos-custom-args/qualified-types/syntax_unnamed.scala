case class Box[T](x: T)

def greater(x: Int with x > 0, y: Int with x > y, z: Int): Boolean = true

@main def Test =
  val v0: (Int with v0 > 0) | (String with v0 == "foo") = ???

  val v1: Int with v1 > 0 = ???
  val v2: (Int with v2 > 0) = ???
  val v3: (Int with v3 > 0) & (Int with v3 < 10) = ???
  val v4: (Int with v4 > 0) & Int with v4 < 10 = ???
  val v4b: Int & String with false = ???
  val v5: ((Int with v5 > 0) & Int) with v5 < 10 = ???
  val v6: (Int with v6 > 0) with v6 < 10 = ???
  val v7: ((Int with v7 > 0) with v7 < 10) = ???
