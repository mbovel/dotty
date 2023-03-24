import annotation.refined

type Pos = Int @refined[Int](_ > 0)

type Neg = Int @refined[Int](it => it < 0)

def secondGreater(x: Int, z: Int , y: Int @refined[Int](_ => x > z)) = ???

def id[T](x: T): T = x

def test() =
  val x1: Pos = 1
  val x2: Int @refined[Int](_ > 0) = 1
  val x3: Int = id[Int @refined[Int](it => it < 0)](1) + id[Neg](-1)

  def f: Pos => Int = ???
  val g: Int => Pos = f

/* TODO: FIXME
type LeftNested = (Int @refined[Int](y => y > 0)) @refined[(Int @refined[Int](y => y > 0))](x => x < 10)
*/
