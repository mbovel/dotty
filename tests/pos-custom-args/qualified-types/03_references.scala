def main =
  def f() =
    val v1: Int = ???
    val v2: {v: Int with v == v1} = v1
    val v3: {v: Int with v1 == v} = v1

    val v4: {v: Int with v == v1} = v2
    val v5 /*: {v: Int with v == v1} */ = v1
    val v6: {v: Int with v == v1} = v5

    val v7 = 10
    val v8 = v7
    val v9: {v: Int with v > 0} = v8

    val v10: {v: Int with v < v7} = 5
    val v11: {v: Int with v == v7 * 2} = 20

    v7

  val y = f()
