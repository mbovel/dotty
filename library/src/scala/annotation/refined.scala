package scala.annotation

/** Refined annotation.
 */
@experimental
class refined[T](predicate: T => Boolean) extends StaticAnnotation
