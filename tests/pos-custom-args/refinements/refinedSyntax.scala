
type Useless = {x: Int with true}

type Pos = {x: Int with x > 0}

type Neg = {x: Int with x < 0}


def secondGreater1(x: Int, z: Int, y: {w: Int with x > z}) = ???

def id[T](x: T): T = x

def test() =
  val x1: Pos = 1
  val x2: {x: Int with x > 0} = 1
  val x3: Int = id[{x: Int with x < 0}](1) + id[Neg](-1)

  def f: Pos => Int = ???
  val g: Int => Pos = f

type Nesting = {x: Int with { val y: {z: Int with z > 0} = ??? ; x > y }}


// Shortcuts:


type Pos2 =
  x: Int with x > 0

/*
def foo(x: Int):
  res: Int with res > 0
= ???

def secondGreater2(x: Int, z: Int, y: Int with x > z) = ???

id[x: Int with x < 0](1)
 */
