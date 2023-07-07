package example

type Useless1 = Int with (_ => true)

type Useless2 = Int with true


type Pos = Int with _ > 0

type Neg = Int with (_ < 0)

//TODO: Fix me
//def secondGreater(x: Int, z: Int, y: Int with x > z) = ???

def id[T](x: T): T = x

def test() =
  val x1: Pos = 1
  val x2: Int with _ > 0 = 1
  val x3: Int = id[Int with _ < 0](1) + id[Neg](-1)

  def f: Pos => Int = ???
  val g: Int => Pos = f

type Nesting = Int with { val y: Int with _ > 0 = ??? ; _ > y }



// Shouldn't work:
type Pos2 = Int with _ + 1 match { case x => true }
