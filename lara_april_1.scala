// Lara April 1, 2025

// 1. Flow-sensitive assumptions + Equality Context

enum MyList[+T]:
  case Cons(head: T, tail: MyList[T])
  case Nil

def myLength(xs: MyList[Int]): Int =
  xs match
    case MyList.Nil =>
      // Add assumption xs == MyList.Nil
      0
    case MyList.Cons(_, xs1) =>
      // Add assumption xs == MyList.Cons(?, xs1)

      // Add assumption xs match {MyList.Cons(_, `xs1`) => true; _ => false}

      // Add assumption xs.isInstanceOf[MyList.Cons[Int]] && head(xs) == ? && tail(xs) == xs1
      1 + myLength(xs1)


// 2. Methods unfolding, improving normalization/evaluation

trait Bijection[A,B]:
  def f(a: A): B
  def g(b: B): A
  def gf_id(a: A): Unit with g(f(a)) == a
  def fg_id(b: B): Unit with f(g(b)) == b

class PlusMinusOne extends Bijection[Int, Int]:
  def f(a: Int): Int = a + 1
  def g(b: Int): Int = b - 1
  def gf_id(a: Int) = ()
  def fg_id(b: Int) = ()


// 3.
