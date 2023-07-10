package dotty.tools
package dotc
package refine

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*

/** A qualified type. This is internally represented as an annotated type with a @retains
 *  or @retainsByName annotation, but the extractor will succeed only at phase CheckQualifiedTypes.
 *  That way, we can ignore refinements information until phase CheckQualifiedTypes since it is
 *  wrapped in a plain annotation.
 */
object QualifiedType:
  /** An extractor that succeeds only during CheckQualifiedTypes.
   */
  def unapply(tp: Type)(using Context): Option[(Type, Tree)] = tp match
    case tp: AnnotatedType
      if ctx.phase == Phases.checkQualifiersPhase
         && tp.annot.symbol == defn.QualifiedAnnot =>
      EventuallyQualifiedType.unapply(tp)
    case _ => None


/** An extractor for types that will be refinement types at phase CheckQualifiedTypes.
 */
object EventuallyQualifiedType:
  def unapply(tp: AnnotatedType)(using Context): Option[(Type, Tree)] =
    val sym = tp.annot.symbol
    if sym == defn.QualifiedAnnot then Some((tp.parent, tp.annot.argument(0).get))
    else None
