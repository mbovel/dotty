package dotty.tools
package dotc
package refine

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*

/** A refinement type. This is internally represented as an annotated type with a @retains
 *  or @retainsByName annotation, but the extractor will succeed only at phase CheckRefinements.
 *  That way, we can ignore refinements information until phase CheckRefinements since it is
 *  wrapped in a plain annotation.
 */
object RefinementType:
  /** An extractor that succeeds only during CheckRefinements.
   */
  def unapply(tp: Type)(using Context): Option[(Type, Tree)] = tp match
    case tp: AnnotatedType
      if ctx.phase == Phases.checkRefinementsPhase
         && tp.annot.symbol == defn.RefinedAnnot =>
      EventuallyRefinementType.unapply(tp)
    case _ => None


/** An extractor for types that will be refinement types at phase CheckRefinements.
 */
object EventuallyRefinementType:
  def unapply(tp: AnnotatedType)(using Context): Option[(Type, Tree)] =
    val sym = tp.annot.symbol
    if sym == defn.RefinedAnnot then Some((tp.parent, tp.annot.argument(0).get))
    else None
