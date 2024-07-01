package dotty.tools.dotc
package qualifiers

import core.Contexts.{ctx, Context}
import core.Phases
import core.Types.{AnnotatedType, Type}

/** A qualified type is internally represented as a type annotated with a
  * [[QualifiedAnnotation]]. This object provides a constructor and an extractor
  * to abstract over the internal representation.
  */
object QualifiedType:
  /** Creates a new qualified type.
    *
    * Note: this is called from [[AnnotatedType.apply]], which ensures that any
    * `@qualified` annotation on a type becomes a [[QualifiedAnnotation]].
    *
    * @param parent
    *   the parent type
    * @param pred
    *   the qualifier expression
    * @return
    *   a new qualified type
    */
  def apply(parent: Type, pred: QualifierExpr)(using Context): AnnotatedType =
    AnnotatedType(parent, QualifiedAnnotation(pred, parent))

  /** Extractor for qualified types that only succeeds during the phase
    * [[CheckQualifiedTypes]]. This allows specialized handling of qualified
    * types during this phase only, while just treating them as standard
    * annotated types in other phases. See [[EventuallyQualifiedType]] for a an
    * extractor that works in all phases.
    *
    * @param tp
    *   the type to deconstruct
    * @return
    *   a pair containing the parent type and the qualifier expression on
    *   success, [[None]] otherwise
    */
  def unapply(tp: Type)(using Context): Option[(Type, QualifierExpr)] =
    if ctx.phase != Phases.checkQualifiersPhase then None
    else EventuallyQualifiedType.unapply(tp)

object EventuallyQualifiedType:
  /** Extractor for qualified types.
    *
    * @param tp
    *   the type to deconstruct
    * @return
    *   a pair containing the parent type and the qualifier expression on
    *   success, [[None]] otherwise
    */
  def unapply(tp: Type)(using Context): Option[(Type, QualifierExpr)] =
    tp match
      case AnnotatedType(parent, QualifiedAnnotation(pred, _)) => Some((parent, pred))
      case _                                                   => None
