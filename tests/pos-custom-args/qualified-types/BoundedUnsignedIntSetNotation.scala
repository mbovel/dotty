import language.experimental.setNotation

type Nat = {x: Int with x >= 0}

extension (x: Boolean)
  def ==> (y: Boolean) = y || !x
  def <==> (y: Boolean) = (x ==> y) && (y ==> x)


trait BoundedUnsignedInt[A]:
  val bound: A
  val natBound: Nat = asNat(bound)

  type BoundedNat = {x: Nat with x <= natBound}

  def asNat(x: A): Nat
  def fromNat(x: BoundedNat): A

  extension (x: A)
    def toNat: BoundedNat = {prop1(x); asNat(x)} // prop1 allows us to narrow a Nat from asNat to a BoundedNat

  // erased ?
  def prop1(x: A):          Unit with asNat(x) <= natBound
  def prop2(x: A):          Unit with fromNat(x.toNat) == x
  def prop3(x: BoundedNat): Unit with fromNat(x).toNat == x

  def prop4(x: A): Unit with asNat(x) == x.toNat = () // Should not need any proof, as it is true by construction

object BoundedUnsignedInt:

  def bound[A : BoundedUnsignedInt]: A = summon[BoundedUnsignedInt[A]].bound
  def natBound[A : BoundedUnsignedInt]: Nat = summon[BoundedUnsignedInt[A]].natBound


  def fits[A : BoundedUnsignedInt](op: (Int, Int) => Int)(x: A, y: A): Boolean =
    val res = op(x.toNat, y.toNat)
    0 <= res && res <= natBound

  // original
  //def relatedOps[A](using BoundedUnsignedInt[A])(iop: (Int, Int) => Int)(bop: (x: A, y: A with fits(iop)(x, y)) => Int ): // (x, y: Nothing) => x
  //  (x: A, y: A) => Unit with (fits(iop)(x, y) ==> ( bop(x, y).toNat == iop(x.toNat, y.toNat) ))

  // simplified, but equivalent ?
  def relatedOps[A]
    (using BoundedUnsignedInt[A]) // Can't use context bound, as it desugars as last parameter instead of first
    (iop: (Int, Int) => Int)
    (bop: (A, A) => A)
    (x: A, y: A with fits(iop)(x, y)): Boolean =
      bop(x, y).toNat == iop(x.toNat, y.toNat)

trait BoundedUnsignedIntWithOps[A](using val base: BoundedUnsignedInt[A]):
  import BoundedUnsignedInt.*

  def add(x: A, y: A with fits(_ + _)(x, y)): A
  def sub(x: A, y: A with fits(_ - _)(x, y)): A
  def lessThan(x: A, y: A): Boolean

  def prop1(x: A, y: A with fits(_ + _)(x, y)): Unit with relatedOps[A](_ + _)(add)(x, y)
  def prop2(x: A, y: A with fits(_ - _)(x, y)): Unit with relatedOps[A](_ - _)(sub)(x, y)
  def prop3(x: A, y: A): Unit with lessThan(x, y) <==> (x.toNat < y.toNat)
  def prop4(x: A): Unit with fits(_ - _)(bound, x)

  // Warning: all + and - before this refer to Int.+, Int.-
  extension (x: A)
    def + (y: A) = add(x, y)
    def - (y: A) = sub(x, y)

  given Ordering[A] = (x: A, y: A) =>
    if lessThan(x, y) then
      -1
    else if lessThan(y, x) then
      1
    else
      0
