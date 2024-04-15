package dotty.tools
package dotc
package qualifiers

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*, Decorators.*
import Annotations.{Annotation, ConcreteAnnotation}

object QualifiedType:
  def apply(parent: Type, pred: QualifierExpr)(using Context): AnnotatedType =
    AnnotatedType(parent, QualifiedAnnotation(pred, parent))

  /** An extractor that succeeds only during the phase CheckQualifiedTypes. */
  def unapply(tp: Type)(using Context): Option[(Type, QualifierExpr)] =
    if ctx.phase != Phases.checkQualifiersPhase then None
    else EventuallyQualifiedType.unapply(tp)

object EventuallyQualifiedType:
  /** An extractor for types that will be qualified types at phase CheckQualifiedTypes. */
  def unapply(tp: Type)(using Context): Option[(Type, QualifierExpr)] =
    tp match
      case AnnotatedType(parent, QualifiedAnnotation(pred, _)) => Some((parent, pred))
      case _                                                => None
