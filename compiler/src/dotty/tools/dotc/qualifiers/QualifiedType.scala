package dotty.tools
package dotc
package qualifiers

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*

object QualifiedType:
  def apply(parent: Type, pred: QualifierExpr)(using Context): Type =
    AnnotatedType(parent, QualifiedAnnotation(pred))

  /** An extractor that succeeds only during CheckRefinements.
    */
  def unapply(tp: Type)(using Context): Option[(Type, QualifierExpr)] =
    if ctx.phase == Phases.checkQualifiersPhase then
      EventuallyQualifiedType.unapply(tp)
    else None

/** An extractor for types that will be refinement types at phase CheckRefinements.
  */
object EventuallyQualifiedType:
  def unapply(tp: Type)(using Context): Option[(Type, QualifierExpr)] =
    tp match
      case tp: AnnotatedType if tp.annot.symbol == defn.QualifiedAnnot =>
        tp.annot match
          case QualifiedAnnotation(pred) => Some((tp.parent, pred))
          case _                         => Some((tp.parent, QualifierExprs.fromClosure(tp.annot.argument(0).get)))
      case _ => None
