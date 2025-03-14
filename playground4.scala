
import language.experimental.modularity

//type Pos = {v: Int with v >= 0}
class Vec(tracked val size: Int)

def le(a: Int, b: Int with a <= b): Unit = ???

def zip[T <: Int, S <: Int](v1: Vec, v2: Vec, w: {x: Unit with v1.size == v2.size}): Vec = ???

def main =
  val v2 = Vec(1)
  val v3 = Vec(1)
  summon[Unit <:< {u: Unit with v2.size == v3.size}]
  val unitxxx = ()
  zip(v2, v3, unitxxx)

  le(1, 2)
