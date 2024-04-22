import language.experimental.setNotation

@main def TestInt: Unit =
  val v1: Int = 10
  val v3: {v: Int with v == 10} = v1.runtimeChecked
