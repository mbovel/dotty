import language.experimental.setNotation

@main def Test: Unit =
  val v1: Int = 10
  try
   val v3: {v: Int with v == 9} = v1.runtimeChecked
  catch
    case e: Throwable => println(e)
