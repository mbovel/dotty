def test =
  val x: Int = ???
  val y: Int = ???
  summon[{v: Int with v == x} <:< {v: Int with v == x}]
