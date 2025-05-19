case class Box[T](x: T)

abstract class Test:
  val v1: Int with v1 > 0
  val v2: (Int with v2 > 0)
  val v3: (Int with v3 > 0) & Int with v3 < 10
  val v4: (Int with v4 > 0) & (Int with v4 < 10)
  val v5: (Int with v5 > 0) with v5 < 10
  val v6: ((Int with v6 > 0) with v6 < 10)
