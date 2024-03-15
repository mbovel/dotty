package dotty.tools.dotc.qualifiers
package solver

import dotty.tools.dotc.core.Contexts.Context

import QualifierExpr.*

final class NoChecksQualifierSolver(using Context) extends NaiveQualifierSolver:
  override protected def leafImplies(from: QualifierExpr, to: QualifierExpr, frozen: Boolean): Boolean =
    if frozen then super.leafImplies(from, to, frozen) else !from.equiv(True)
