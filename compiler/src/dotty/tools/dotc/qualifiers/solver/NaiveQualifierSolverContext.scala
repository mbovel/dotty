package dotty.tools.dotc.qualifiers

package solver

import annotation.tailrec

import QualifierExpr.ordering.{min, max}
import QualifierExpr.*

import NaiveQualifierSolverContext.*

private[solver] final class NaiveQualifierSolverContext(
    val premise: QualifierExpr = True,
    classes: EqClassMap = Map.empty
):
  def assume(x: QualifierExpr): NaiveQualifierSolverContext = assumeImpl(classes, and(x, premise))
  def rewrite(x: QualifierExpr): QualifierExpr = rewriteImpl(classes, x)
  def nClasses() = classes.valuesIterator.distinct.size
  override def toString() = f"FactsState(\npremise = $premise,\nclasses = ${eqClassMapToString(classes)}\n)"

private object NaiveQualifierSolverContext:
  type EqClassMap = Map[QualifierExpr, EqClass]

  case class EqClass(repr: QualifierExpr, members: Set[QualifierExpr]):
    def add(x: QualifierExpr) = EqClass(min(repr, x), members + x)
    def merge(that: EqClass) = EqClass(min(repr, that.repr), members ++ that.members)

  def reprImpl(classes: EqClassMap, x: QualifierExpr): QualifierExpr =
    classes.get(x) match
      case Some(c) => c.repr
      case None    => x

  @tailrec
  def assumeImpl(classes: EqClassMap, x: QualifierExpr): NaiveQualifierSolverContext =
    val eqs = topLevelEquals(x)
    if eqs.isEmpty then
      NaiveQualifierSolverContext(x, classes)
    else
      var newClasses = classes
      for Equal(x, y) <- eqs do
        newClasses = addEq(newClasses, x, y)
      val newExpr = rewriteImpl(newClasses, x)
      assumeImpl(newClasses, newExpr)

  @tailrec
  def rewriteImpl(classes: EqClassMap, expr: QualifierExpr): QualifierExpr =
    val res = expr.map(reprImpl(classes, _))
    if res == expr then expr else rewriteImpl(classes, res)

  def addEq(classes: EqClassMap, x: QualifierExpr, y: QualifierExpr): EqClassMap =
    if x == y then
      classes
    else
      val xClass = classes.getOrElse(x, EqClass(x, Set(x)))
      val yClass = classes.getOrElse(y, EqClass(y, Set(y)))
      if xClass == yClass then
        classes
      else
        val mergedClass = xClass.merge(yClass)
        classes ++ mergedClass.members.map(_ -> mergedClass)

  def topLevelEquals(x: QualifierExpr): List[Equal] =
    x match
      case x: Equal  => List(x)
      case And(args) => args.flatMap(topLevelEquals)
      case _         => Nil

  def eqClassMapToString(classes: EqClassMap) =
    classes
      .valuesIterator
      .distinct
      .map(c => c.repr.toString() + " -> " + c.members.mkString("{", ", ", "}"))
      .mkString("{", ", ", "}")
