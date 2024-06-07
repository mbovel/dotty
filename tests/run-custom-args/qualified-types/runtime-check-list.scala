import language.experimental.setNotation

type Pos = {x: Int with x >= 0}

@main def Test: Unit =
  val x: Int = 10
  val y: Pos = x.runtimeChecked
  val l: List[Int] = List(0, 2, 3)
  val l2: List[Pos] = l.runtimeChecked
  val l3: List[Pos] = l.map(x => x.runtimeChecked)
