def p(x: Int): Boolean = ???
def q(x: Int): Boolean = ???

def f(a: Int, b: Int with a == b): {res: Int with b == res} = ???

def demo =
  val a1: {v: Int with v == 0 && p(v)} = ???
  val a2 = a1
  val b1: {v: Int with v == 0 && q(v)} = ???
  val res = f(a2, b1)
