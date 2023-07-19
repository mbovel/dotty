package dotty.tools
package dotc
package qualifiers
package solver

import core.*
import Symbols.*, util.Spans.NoCoord

import org.junit.Test
import org.junit.Assert.*

import QualifierExpr.*

trait QualifierSolverTest(val solver: QualifierSolver):
  val v = Var(1)
  val v2 = Var(2)
  val v3 = Var(3)
  val x = LambdaArg(0, 0)
  val y = LambdaArg(0, 1)

  extension (p: QualifierExpr) def tryImply(q: QualifierExpr) = solver.tryImply(p, q)

  @Test def trueCannotImplyFalse =
    assertFalse(True.tryImply(False))

  @Test def falseCanImplyTrue =
    assert(False.tryImply(True))

  @Test def varCanImplyItself =
    assert(v.tryImply(v))

  @Test def varCanImplyVar =
    assert(Var(1).tryImply(Var(2)))

  @Test def trueCannotImplyEq =
    assertFalse(True.tryImply(Equal(x, y)))

  @Test def trueVarCannotImplyEq =
    assert(True.tryImply(v))
    assertFalse(v.tryImply(Equal(x, y)))

  @Test def canBacktrack =
    assert(v2.tryImply(Equal(x, y))) // v2 is not empty
    assert(True.tryImply(or(and(v, v2), v3)))
    assert(v.tryImply(Equal(x, y))) // v is not empty
