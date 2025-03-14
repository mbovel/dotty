package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.core.Atoms
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.Type

/** Extractor that matches a type containing a single atom and returns that
 *  atom.
 *
 *  @see [[Atoms]]
 */
object SingleAtom:
  def unapply(tp: Type)(using Context): Option[Type] =
    tp.atoms match
      case Atoms.Range(lo, hi) =>
        if lo.size == 1 then Some(lo.head)
        else if hi.size == 1 then Some(hi.head)
        else None
      case _ =>
        None
