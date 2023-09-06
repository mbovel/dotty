package dotty.tools.dotc.qualifiers
package utils

import collection.mutable

/**
  * A data structure that maintains a set of equivalence classes of elements of type T.
  *
  * TODO(mbovel): optimize using a union-find data structure?
  * https://chat.openai.com/share/d58208f8-bd46-4526-9aca-ba9108c8d326
  */
class EqClasses[T](using ord: Ordering[T]):
  private class EqClass(var repr: T, val members: mutable.Set[T]):
    def add(x: T): Unit =
      members.add(x)
      repr = ord.min(x, repr)
    def addAll(that: EqClass): Unit =
      members.addAll(that.members)
      repr = ord.min(that.repr, repr)

  private val classes = mutable.Map.empty[T, EqClass]

  private def getEqClass(x: T): EqClass =
    classes.getOrElseUpdate(x, EqClass(x, mutable.Set(x)))

  // Only for testing
  private[utils] def nClasses: Int = classes.valuesIterator.distinct.size

  final def repr(x: T): T =
    if !classes.contains(x) then
      // Avoids creating a new EqClass if x is not in the map
      x
    else
      getEqClass(x).repr

  final def addEq(x: T, y: T): Boolean =
    val xClass = getEqClass(x)
    val yClass = getEqClass(y)
    if xClass != yClass then
      xClass.addAll(yClass)
      for z <- yClass.members do
        classes(z) = xClass
      true
    else
      false

  override def toString(): String =
    classes
      .valuesIterator
      .distinct
      .map(c => c.repr.toString() + " -> " + c.members.mkString("{", ", ", "}"))
      .mkString("{", ", ", "}")
