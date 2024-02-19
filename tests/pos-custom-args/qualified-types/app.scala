def p(x: Int): Boolean = ???
def q(x: Int): Boolean = ???

def f(a: Int, b: Int with a == b): {res: Int with b == res} = ???

def demo =
  val a: {v: Int with v == 0 && p(v)} = ???
  val b: {v: Int with v == 0 && q(v)} = ???
  val res = f(a, b)
