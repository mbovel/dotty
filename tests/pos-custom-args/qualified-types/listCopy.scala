def copy[T](xs: List[T]): {l: List[T] with l.size == xs.size} = ???
def makeList(size: Int): {l: List[Int] with l.size == size} = ???

def demo =
  val three = 3
  val l2 = makeList(three)
  val l3 = copy(l2)
  ()
