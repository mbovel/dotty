def test =
  val l1 = List(1, 2, 3).asInstanceOf[List[{x: Int with x > 0}]]
  val l2 = List(0, 2, 3).asInstanceOf[List[{x: Int with x >= 0}]]
  val l3 = l1 ++ l2
  val l4: List[{x: Int with x >= 0}] = l3
