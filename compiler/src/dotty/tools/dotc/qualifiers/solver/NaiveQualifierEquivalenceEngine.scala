package dotty.tools.dotc.qualifiers

package solver

import annotation.tailrec

import QualifierExpr.ordering.lt
import QualifierLogging.log
import QualifierExpr.*

import NaiveQualifierEquivalenceEngine.*

private[solver] final class NaiveQualifierEquivalenceEngine private(
    val premise: QualifierExpr = True,
    classes: EqClasses = Map.empty
):
  def assume(x: QualifierExpr): NaiveQualifierEquivalenceEngine = assumeImpl(classes, and(x, premise))
  def rewrite(x: QualifierExpr): QualifierExpr = rewriteImpl(classes, x)
  def nClasses() = classes.valuesIterator.map(reprImpl(classes, _)).distinct.size
  override def toString() = f"NaiveQualifierEquivalenceEngine(\npremise = $premise,\nclasses = $classes}\n)"

object NaiveQualifierEquivalenceEngine:
  type EqClasses = Map[QualifierExpr, QualifierExpr]

  def apply(): NaiveQualifierEquivalenceEngine = new NaiveQualifierEquivalenceEngine()
  def apply(premise: QualifierExpr): NaiveQualifierEquivalenceEngine = assumeImpl(Map.empty, premise)

  @tailrec
  private def reprImpl(classes: EqClasses, x: QualifierExpr): QualifierExpr =
    classes.get(x) match
      case Some(c) => reprImpl(classes, c)
      case None    => x

  @tailrec
  private def assumeImpl(classes: EqClasses, x: QualifierExpr): NaiveQualifierEquivalenceEngine =
    val xRewritten = rewriteImpl(classes, x)
    val eqs = topLevelEquals(xRewritten)
    if eqs.isEmpty then
      new NaiveQualifierEquivalenceEngine(xRewritten, classes)
    else
      var newClasses = classes
      for Equal(y, z) <- eqs do
        newClasses = addEq(newClasses, y, z)
      newClasses = rewriteClassMap(newClasses)
      assumeImpl(newClasses, x)

  @tailrec
  private def rewriteImpl(classes: EqClasses, expr: QualifierExpr): QualifierExpr =
    log(f"rewriteImpl($classes, $expr)")
    val res = expr.map(e => reprImpl(classes, e.shallowNormalize()))
    if res == expr then expr else rewriteImpl(classes, res)

  private def addEq(classes: EqClasses, x: QualifierExpr, y: QualifierExpr): EqClasses =
    if x == y then
      classes
    else
      val xRepr = reprImpl(classes, x)
      val yRepr = reprImpl(classes, y)
      if xRepr == yRepr then classes
      else if lt(xRepr, yRepr) then classes + (yRepr -> xRepr)
      else classes + (xRepr -> yRepr)

  @tailrec
  private def rewriteClassMap(classes: EqClasses): EqClasses =
    var newClasses = classes
    for from <- classes.keysIterator do
      newClasses = addEq(newClasses, from, rewriteImpl(newClasses, from))
    if newClasses eq classes then classes else rewriteClassMap(newClasses)

  private def topLevelEquals(x: QualifierExpr): List[Equal] =
    x match
      case x: Equal  => List(x)
      case And(args) => args.flatMap(topLevelEquals)
      case _         => Nil
