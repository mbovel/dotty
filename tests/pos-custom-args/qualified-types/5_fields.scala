
class EqualPair(val x: Int, val y: Int with x == y)

def main =
  val p = EqualPair(42, 42)
  val v1: Int with v1 == p.y = p.x
