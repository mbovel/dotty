trait Bijection[A,B]:
  def f(a: A): {r: B with g(r) == a}
  def g(b: B): {r: A with f(r) == b}

class PlusMinusOne extends Bijection[Int, Int]:
  def f(a: Int) = a + 1
  def g(b: Int) = b - 1
