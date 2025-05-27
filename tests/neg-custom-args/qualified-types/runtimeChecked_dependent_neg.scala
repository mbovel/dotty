@main def Test =
  val res = foo((1: Int), 2.runtimeChecked) // error
