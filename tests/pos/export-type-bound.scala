trait M1:
  trait A
  trait F[T<:A]

object M2 extends M1

trait Test:
  export M2.*
  def y: F[A]
