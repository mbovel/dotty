def makeList(size: Int): {l: List[Int] with l.size == size} = ???

def demo =
  val three = 3
  val l2: {l: List[Int] with l.size == 3} = makeList(three)

