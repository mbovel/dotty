import annotation.qualified
def id(x: Int): Int @qualified[Int](v => v == x) = x
def equal(x: Int, y: {v: Int with v == x}): Int = y

def main(v6: Int) =

  val v1: {v: Int with v == 2} =
    v6 match
      case v: {v: Int with v == 2} => v
      case _ => throw new Error()
