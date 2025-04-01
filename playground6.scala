def le(a: Int, b: Int with a <= b): Unit = ???

def main =
  val x: Int = ???
  val y: Int with x <= y = ???
  le(x, y)

def main2 =
  val z: {v: Int with v > 0} = ???
  summon[z.type <:< {v: Int with v == z}]
