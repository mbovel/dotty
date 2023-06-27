import language.experimental.postfixLambda


type Shape = List[Int] with _.forall(_ > 0)

extension (s: Shape)
  //erased ?
  def nbrElems = s.fold(1)(_ * _)
  //erased ?
  def reduce(axes: Shape) = s.zipWithIndex.filter((_, i) => axes.contains(i)).map(_._1)

trait Tensor[T]:
  val shape: Shape

  //erased ?
  def sameShape(t: Tensor[T]): Boolean = shape.corresponds(t.shape)(_ == _)

  // TODO: Fix when https://github.com/lampepfl/dotty/issues/18064 is fixed
  def add(t: Tensor[T] with t.sameShape(this)): Tensor[T] //with _.sameShape(this)

  // TODO: Fix when https://github.com/lampepfl/dotty/issues/17939 is fixed
  def mean(axes: Shape): Tensor[T] //with _.shape == shape.reduce(axes)
  def reshape(newShape: Shape with newShape.nbrElems == shape.nbrElems): Tensor[T] //with _.shape == newShape

class TensorImpl[T](val shape: Shape) extends Tensor[T]:
  def add(t: Tensor[T] with t.sameShape(this)) = this
  def mean(axes: Shape) = TensorImpl(shape.reduce(axes))
  def reshape(newShape: Shape with newShape.nbrElems == shape.nbrElems) = TensorImpl(newShape)


inline def compiletimeAssert(inline e: Boolean): Unit =
  (): (Unit with e)

@main def test() =
  val shape = List(5, 6, 2)

  val a1 = (): Unit with shape.size == 60
  val a2 = (): Unit with shape.reduce(List(0)) == List(6,2)
  val a3 = (): Unit with shape.reduce(List(0, 2)) == List(6)
  val a4 = (): Unit with shape.reduce(List(1, 6)) == List(5,2)

  // Should be equivalent to above
  compiletimeAssert(shape.size == 60)
  compiletimeAssert(shape.reduce(List(0)) == List(6,2))
  compiletimeAssert(shape.reduce(List(0, 2)) == List(6))
  compiletimeAssert(shape.reduce(List(1, 6)) == List(5,2))

  val t1 = TensorImpl[Int](List(5, 6, 2))
  val t2 = TensorImpl[Int](List(5, 6, 2))
  val t3 = t1.add(t2)
  val t4 = t1.mean(List(0,2))
  val t5 = t1.reshape(List(2, 6, 5))


object unused:

  type NonEmpty = Shape with _.size > 0

  infix type #:[H <: Int & Singleton, T <: Shape] = NonEmpty with l => l.head.isInstanceOf[H] && l.tail.isInstanceOf[T]

  type EmptyShape = Shape with _.size == 0
  val EmptyShape: EmptyShape & Singleton = Nil

  object Tmp:
    extension (h: Int)
      def #:(t: Shape): (h.type #: t.type) = h :: t
  import Tmp.#:

  object `#:` :
    infix def unapply(l: Shape with l.size > 0): // Irrefutable !
      (Int, Shape with _.size + 1 == l.size)
      =
        (l.head, l.tail)
    /*
    infix def unapply(l: Shape): Option[(Int, Shape)] = l match
      case h :: t => Some((h, t)) // will it infer t is indeed a Shape ?
      case Nil    => None
    */



  def contains(s: Shape, n: Int): Boolean with _ == s.contains(n) = ???
