import language.experimental.setNotation

@main def Test: Unit =
  val v1: Int = 10
  val v2: Int = 9
  val v3: {v: Int with v == 10} = v1.runtimeChecked
  val v4: {v: Int with v == 10} = v2.runtimeChecked // error
