package dotty.tools
package dotc
package qualifiers
package solver

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*
import scala.collection.mutable
import QualifierExpr.*
import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.core.Constants.Constant

abstract class QualifierSolver:
  def tryImply(p: QualifierExpr, q: QualifierExpr): Boolean
  def instantiate(p: QualifierExpr): QualifierExpr

  var maxVarIndex: Int = 0
  def freshVar(): Var =
    maxVarIndex = maxVarIndex + 1
    Var(maxVarIndex)

  def constPred(c: Constant) =
    if QualifierExpr.fromConst.isDefinedAt(c) then
      Equal(QualifierExpr.fromConst(c), QualifierExpr.predArg)
    else
      True
