class Prop(fact: {v: Boolean with v})

trait Bijection[A,B]:
  def f(a: A): B
  def g(b: B): A
  def gf_id(a: A): Prop {val fact: Boolean with g(f(a)) == a}
  def fg_id(b: B): Prop {val fact: Boolean with f(g(b)) == b}

class PlusMinusOne extends Bijection[Int, Int]:
  def f(a: Int): Int = a + 1
  def g(b: Int): Int = b - 1
  def gf_id(a: Int): Prop {val fact: Boolean with g(f(a)) == a} = Prop(g(f(a)) == a)
  def fg_id(b: Int): Prop {val fact: Boolean with f(g(b)) == b} = Prop(f(g(b)) == b)
