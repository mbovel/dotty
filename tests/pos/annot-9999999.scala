import scala.annotation.qualified
import scala.annotation.experimental

class local[T](predicate: T => Boolean) extends annotation.RefiningAnnotation

def le(a: Int, b: Int @local[Int](b => a <= b)): Unit = ???

def le2(a: Int, b: Int with a <= b): Unit = ???

def main =
  le(1, 2)
  le2(1, 2)
