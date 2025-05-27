def foo(x: Int, y: {v: Int with v > x}): y.type = y

def getInt(): Int = 1

type Pos = {v: Int with v > 0}
type Neg = {v: Int with v < 0}

import scala.reflect.TypeTest

/*
given Conversion[Int, Pos] with
  def apply(x: Int): Pos =
    x match
      case x: Pos => x

given [S, T](using TypeTest[S, T]): Conversion[S, T] with
  def apply(x: S): T =
    x match
      case x: T => x
*/

//given Conversion[Pos, String] with
//  def apply(x: Pos): String =
//    x match
//      case x: Pos => "Pos: " + x.toString()
//
//given Conversion[Neg, String] with
//  def apply(x: Neg): String =
//    x match
//      case x: Neg => "Neg: " + x.toString()

@main def Test =
  val v1= foo(1, 2.runtimeChecked)

  val p: Int = 1
  val v2 = foo(p, 0.runtimeChecked)
