enum MyList[+T]:
  case Cons(head: T, tail: MyList[T])
  case Nil

def main =
  def myLength(xs: MyList[Int]): Int =
    xs match
      case MyList.Nil => 0
      case MyList.Cons(_, xs1) => 1 + myLength(xs1)


  val x: Int = ???
  //val y = myLength(MyList.Cons(x, MyList.Cons(2, MyList.Nil)))

  val xs: {l: MyList[Int] with myLength(l) == 2} = MyList.Cons(x, MyList.Cons(2, MyList.Nil))
  ////val ys: {l: List[Int] with length(l) == 2} = x :: 2 :: Nil
