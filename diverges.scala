type Loop[T] = T match
    case List[t] => Loop[List[t]]
    case _ => T

@main def typeCheckDiverges(): Unit =
    val x: Loop[List[Int]] = 34
    val y: Loop[List[Int]] = 34
