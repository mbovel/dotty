import annotation.refined // This should not be necessary

type Useless = Int with _ => true

type Pos = Int with this > 0

type Neg = Int with this < 0

def secondGreater(x: Int, z: Int, y: Int with x > z) = ???

def id[T](x: T): T = x

def test() =
  val x1: Pos = 1
  val x2: Int with this > 0 = 1
  val x3: Int = id[Int with this < 0](1) + id[Neg](-1)

  def f: Pos => Int = ???
  val g: Int => Pos = f

type Nesting = Int with { val y: Int with ??? ; this.x > y.this }
