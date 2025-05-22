def f(x: Int): Int = ???

def test: Unit =
  val x: Int = ???
  val y: Int = ???
  val z: Int = ???

  val v0: {v: Int with v == 2 + (x * y * y * z)} = (x * y * z * y) + 2
  //val v1: {v: Int with v == x + 1} = 1 + x
  //val v2: {v: Int with v == y + x} = x + y
  //val v3: {v: Int with v == x + 2} = 1 + x + 1
  //val v4: {v: Int with v == x + 2} = 1 + (x + 1)
  //val v5: {v: Int with v == x + 2 * y} = y + x + y
  //val v6: {v: Int with v == x + 2 * y} = y + (x + y)
  //val v7: {v: Int with v == x + 3 * y} = 2 * y + x + y
  //val v8: {v: Int with v == x + 3 * y} = 2 * y + (x + y)
  //val v9: {v: Int with v == 0} = 1 - 1
  ////val v10: {v: Int with v == 0} = x - x // StackOverflowError
  //val v11: {v: Int with v == 0} = x + (x * -1)
  ////val v12: {v: Int with v == x} = 1 + x - 1 // StackOverflowError
  //val v13: {v: Int with v == 4 * (x + 1)} = 2 * (x + 1) + 2 * (1 + x)
  //val v14: {v: Int with v == 4 * (x / 2)} = 2 * (x / 2) + 2 * (x / 2)

