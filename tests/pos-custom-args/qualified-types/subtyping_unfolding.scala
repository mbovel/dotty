def test: Unit =
  val x: Int = ???
  val y: Int = x + 1

  //summon[{v: Int with v == x + 1} <:< {v: Int with v == y}] // unfolded too late
  //summon[{v: Int with true} <:< {v: Int with v == y}] // unfolded too late
  //summon[{v: Int with v == y} <:< {v: Int with v == x + 1}] // unfolded too late
