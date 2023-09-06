

def concat[T](xs: List[T], ys: List[T]): {l: List[T] with l.size == xs.size + ys.size} =
  (xs ++ ys).asInstanceOf

def test =
  val l1 = List(1, 2).asInstanceOf[{l: List[Int] with l.size == 2}]
  val l2 = List(0, 2, 3).asInstanceOf[{l: List[Int] with l.size == 3}]
  val l3 = concat(l1, l2)
  val l4: {l: List[Int] with l.size == 5} = l3
