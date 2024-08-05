import language.experimental.setNotation

import annotation.qualified

def return10(): {v: Int with v == 10} = 10
def require12(x: {v: Int with v == 12}): Int = x

def id(x: Int): Int @qualified[Int](v => ((v2: Int) => v2 == x)(v)) = x
def equal(x: Int, y: {v: Int with v == x}): Int = y

def myApply(x: Int)(f: {y: Int with y == x} => Unit) = f(x)

def Main =
  val v1: {v: Int with v == 10} = return10()
  val v2 = return10()
  val v3: {v: Int with v == 10} = v2

  require12(12)
  val v4 = 12
  require12(v4)

  val v5: {v: Int with v == 15} = id(15)
  val v6 = id(15)
  val v7: {v: Int with v == 15} = v6

  equal(10, v1)

  myApply(10): (x: {y: Int with y == 10}) =>
    val z: Int with z == 10 = x
    ()

  myApply(10): x =>
    val z: Int with z == 10 = x
    ()

  def g(a: Int, b: Int with a < b): Int = 42
  def f(a: Int, b: Int with a < b): Int = g(a, b)



