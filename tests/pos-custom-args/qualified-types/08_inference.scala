def main =
  val v1: {v: Int with v >= 0} = 0
  val v2 = v1
  val v3: {v: Int with v >= 0} = v2

  val v4: Int = 0
  val v5 = v4
  val v6 = v5
  val v7 = v6
