enum MyList[+T]:
  case Cons(head: T, tail: MyList[T])
  case Nil

transparent inline def myLength(xs: MyList[Int]): Int =
  inline xs match
    case MyList.Nil => 0
    case MyList.Cons(_, xs1) => 1 + myLength(xs1)

def main =
  val x: Int = ???
  val y = myLength(MyList.Cons(x, MyList.Cons(2, MyList.Nil: MyList.Nil.type): MyList.Cons[Int]): MyList.Cons[Int])
