import annotation.qualified
def id(x: Int): Int @qualified[Int](v => v == x) = x
def equal(x: Int, y: {v: Int with v == x}): Int = y

def main =
  val v1 = id(10)
  val v2: {v: Int with v > 0} = v1

  val v3: {v: Int with v > 0} = 10
  val v4 = id(v3)
  val v5: {v: Int with v > 0} = v4

  val v6 = 10
  val v7 = 10
  equal(v6, v7)
