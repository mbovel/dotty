import language.experimental.setNotation

@main def TestInt: Unit =
  val v1: List[Int] = List(1,2,3)
  val v3: {v: List[Int] with v.size == 3} = v1.runtimeChecked
