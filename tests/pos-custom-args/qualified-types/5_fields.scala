
class EqualPair(val x: Int, val y: Int with x == y)

def main =
  val v1 = EqualPair(42, 42)
