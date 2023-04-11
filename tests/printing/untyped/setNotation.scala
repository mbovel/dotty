package example

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

type Pos3 =
  Int & {x: Int with x > 0}

// Brace ellison

type Pos2 =
  x: Int with x > 0

type Pos5 =
  x: Int with
    val y = x*x
    y > 0


type T = (
  x: Int
) => x.type

/* Doesn't work because of bug with CheckRefinements, see refined.scala
type Pos4 =
  x: {y: Int with y > 0} with x < 10
*/

// Shortcuts:

type Alias = Int with true

def foo(x: Int with true) = ???

def bar(x: Int with x > 0) = ???

def secondGreater2(x: Int, y: Int, z: Int with x > y) = ???
