
def makeList(size: Int): {l: List[Int] with l.size == size} = ???


class MyList[T]:
  val size: Int
  def copy: (List[T] with it.size == size) = ???
  def copy: (List[T] with size == MyList.this.size) = ???

def demo =
  val three = 3
  val l2 = makeList(three)
  val l3 = copy(l2)
  val l4: (l: List[Int]) with (l.size == 3) = l3
  val l5: {val l: List[Int]}
  ()
