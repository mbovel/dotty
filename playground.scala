import scala.language.experimental.modularity

def forall(f: Int => Boolean): Boolean = ???

def opaqueId[T](x: T): T = x

def demo =
  val x: Int = ???
  val y: 42 = 42

  x match
    case `y` => println("y")
    case _ => println("x")

  def f(y2: Int with y2 == 42) = ???

  x match
    case z2: y.type => f(z2)
    case _ => println("x")


  val v2: 42 = 42
  val v3: {it: Int with it == v2} = v2
  val v4: {it: Int with it == 42} = v3

  val v5: 42 = 42
  val v6: v5.type = v5
  val v7: {it: Int with it == 42} = v6

  val v8: Int = ???
  val v9 = v8 + v8
  val v10: {it: Int with it == v8 * 2} = v9

  def id[T](x: T): T = x
  val v11 = id(v9)
  val v12: {it: Int with it == v8 * 2} = v11

  val v13: Int = opaqueId(v9)
  val v14: {it: Int with it == opaqueId(v8 * 2)} = v13

  val v15: Int = opaqueId(v9) + opaqueId(v9)
  val v16: {it: Int with it == 2 * opaqueId(v9)} = v15

  def eq[T](a: T)[S](b: S): Boolean = a == b
  val v17: {it: Boolean with it == (v5 == v8)} = eq(v5)(v8)

  val v18: {v: Int with forall(x => x == 42)} = ??? : {v: Int with forall(y => y == 42)}

  val v19: {it: Int with it == 2} = ??? : {it: Int with 2 == it}
  val v20: {v: Int with forall(x => x == 42)} = ??? : {v: Int with forall(y => 42 == y)}

  // Selfify
  val v21: Int with v21 == 3 = 3

  // Normalize
  val v22: Int with v22 == 3 = ??? : {x: Int with x == 1 + 2}
  val n: Int = ???
  val v23: Int with v23 == n + 1 = ??? : {x: Int with x == 1 + n}

  val four: 4 = 4
  val v24: Int with v24 == 8 = ??? : {x: Int with x == four + four}
  val v25: Int with v25 > 0 = 4

/*


  case class Vec(val n: Int)

  // by default not dependent so we just have
  val v: Vec = Vec(3)

  // make it dependent with `tracked`
  case class VecDep(tracked val n: Int)

  // now we have a precise type for the constructor:
  val v2: VecDep(3) = VecDep(3)

  // Scala has precise types constant and "paths" already.
  // What about arbitrary refinements? Entering qualifie types
  case class VecDepPos(tracked val n: {v: Int with v >= 0})
  //val v3 = VecDepPos(-1) // error
  val v4 = VecDepPos(3) // okay

  def zip(a: VecDepPos, b: VecDepPos(a.n)): VecDepPos(a.n) = ???
  def concat(a: VecDepPos, b: VecDepPos): VecDepPos {val n: {v: Int with v == a.n + b.n}} = ???

  // Selfify
  val v5: Int with v5 == 3 = 3

  // Normalize
  val v6: Int with v6 == 3 = ??? : {x: Int with x == 1 + 2}
  val n: Int = ???
  val v7: Int with v7 == n + 1 = ??? : {x: Int with x == 1 + n}

  val four: 4 = 4
  val v8: Int with v8 == 8 = ??? : {x: Int with x == four + four}
  val v9: Int with v9 > 0 = 4
  //val v10: Int with v9 < 0 = 4


  //val v9: VecDepPos {val n: {v: Int with v == 6}} = concat(v4, v4)
  */
