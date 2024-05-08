package dotty.tools
package dotc
package qualifiers

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*

object QualifiedType:
  def apply(parent: Type, pred: QualifierExpr)(using Context): Type =
    AnnotatedType(parent, QualifiedAnnotation(pred, parent))

  /** An extractor that succeeds only during CheckRefinements.
    */
  def unapply(tp: Type)(using Context): Option[(Type, QualifierExpr)] =
    tp match
      case tp: AnnotatedType if tp.annot.symbol == defn.QualifiedAnnot =>
        tp.annot match
          case QualifiedAnnotation(pred, _) => Some((tp.parent, pred))
          case _ =>
            if ctx.phase == Phases.checkQualifiersPhase then
              Some((tp.parent,  QualifierExprs.fromClosure(tp.annot.argument(0).get)))
            else
              None
      case _ => None

/** An extractor for types that will be refinement types at phase CheckRefinements.
  */
object EventuallyQualifiedType:
  def unapply(tp: Type)(using Context): Option[(Type, Tree)] =
    tp match
      case tp: AnnotatedType if tp.annot.symbol == defn.QualifiedAnnot =>
        tp.annot match
          case QualifiedAnnotation(pred, _) => Some((tp.parent, tp.annot.tree))
          case _                         => Some((tp.parent, tp.annot.argument(0).get))
      case _ => None
