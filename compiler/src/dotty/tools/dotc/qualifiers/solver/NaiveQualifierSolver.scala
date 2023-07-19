package dotty.tools
package dotc
package qualifiers
package solver

import QualifierExpr.*

final class NaiveQualifierSolver extends NoChecksQualifierSolver:
  override protected def leafImplies(from: QualifierExpr, to: QualifierExpr, frozen: Boolean): Boolean =
    from.equiv(to) || from.equiv(False) || to.equiv(True)
