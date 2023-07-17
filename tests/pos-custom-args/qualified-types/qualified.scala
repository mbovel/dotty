import annotation.qualified

type Pos = Int @qualified[Int](_ > 0)

type Neg = Int @qualified[Int](it => it < 0)

def secondGreater(x: Int, z: Int , y: Int @qualified[Int](_ => x > z)) = ???

def id[T](x: T): T = x

def test() =
  val x1: Pos = 1
  val x2: Int @qualified[Int](_ > 0) = 1
  val x3: Int = id[Int @qualified[Int](it => it < 0)](1) + id[Neg](-1)

  def f: Pos => Int = ???
  val g: Int => Pos = f

/* TODO: FIXME
type LeftNested = (Int @qualified[Int](y => y > 0)) @qualified[(Int @qualified[Int](y => y > 0))](x => x < 10)
*/
