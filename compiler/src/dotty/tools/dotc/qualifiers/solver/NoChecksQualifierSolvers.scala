package dotty.tools.dotc.qualifiers
package solver

import QualifierExpr.*

final class NoChecksQualifierSolver extends NaiveQualifierSolver:
  override protected def leafImplies(from: QualifierExpr, to: QualifierExpr, frozen: Boolean): Boolean =
    if frozen then super.leafImplies(from, to, frozen) else !from.equiv(True)
