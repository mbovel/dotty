
class EqualPair(val x: Int, val y: Int with x == y):
  def foo: Int = 42

def getY(q: EqualPair): {v: Int with q.x == v} = q.y

def main =
  val p = EqualPair(42, 42)
  val y = p.y
  val v1: {v: Int with p.x == v} = p.y
  val v2: {v: Int with p.x == v} = getY(p)
  val v3: {v: Int with p.foo == v} = p.foo // No purity checks for now. In the future, should fail if `foo` is not pure.
