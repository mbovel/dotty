def main =
  val v1: {v: Int with v >= 0} = 0
  val v2 = v1
  val v3: {v: Int with v >= 0} = v2
