class myAnnotation extends scala.annotation.RefiningAnnotation

def f[T <: Int @myAnnotation](x: T): T = x

class Box[T <: Int @myAnnotation](val x: T)

def main =
  val x: Int @myAnnotation = ???
  f(x)
  Box(x)

  val y: Int = 22
  f(y) // error: type mismatch
  Box(y) // error: type mismatch
