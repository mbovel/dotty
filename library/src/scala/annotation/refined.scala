package scala.annotation

/** Refined annotation.
 */
@experimental
class refined[T](predicate: Boolean | (T => Boolean)) extends StaticAnnotation
