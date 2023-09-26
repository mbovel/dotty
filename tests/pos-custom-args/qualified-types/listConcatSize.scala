

def concat[T](xs: List[T], ys: List[T]): {l: List[T] with l.size == xs.size + ys.size} =
  (xs ++ ys).asInstanceOf

def zip[T, S](xs: List[T], ys: List[S] with ys.size == xs.size): {l: List[(T, S)] with l.size == xs.size} =
  (xs.zip(ys)).asInstanceOf

def copy[T](xs: List[T]): {l: List[T] with l.size == xs.size} =
  xs.asInstanceOf

def test: Unit =
  val l1 = List(1, 2).asInstanceOf[{l: List[Int] with l.size == 2}]
  val l3 = zip[Int, Int](l1, copy(l1))
  ()
