package dotty.tools.dotc.qualifiers
package solver

import org.junit.Test
import org.junit.Assert.*
import org.junit.FixMethodOrder
import org.junit.runners.MethodSorters

import QualifierExpr.*

@FixMethodOrder(MethodSorters.JVM)
abstract class QualifierSolverTest(solver: QualifierSolver):
  // Note: JUnit4 creates a new instance of the test class before calling each
  // test method. Therefore, we get a fresh solver and fresh expressions for
  // each test.
  val v = solver.freshVar()
  val v2 = solver.freshVar()
  val v3 = solver.freshVar()
  val x = solver.freshRef("x")
  val y = solver.freshRef("y")
  val z = solver.freshRef("z")
  val one = IntConst(1)
  val two = IntConst(2)

  protected def assertImplies(from: QualifierExpr, to: QualifierExpr) =
    assertTrue(s"expected $from to imply $to", solver.tryImply(from, to))

  protected def assertNotImplies(from: QualifierExpr, to: QualifierExpr) =
    assertFalse(s"expected $from not to imply $to", solver.tryImply(from, to))

  @Test def trueCannotImplyFalse =
    assertNotImplies(True, False)

  @Test def falseCanImplyTrue =
    assertImplies(False, True)

  @Test def varCanImplyVar =
    assertImplies(v, v2)

  @Test def trueCannotImplyEq =
    assertNotImplies(True, Equal(x, y))

  @Test def trueVarCannotImplyEq =
    assertImplies(True, v)
    assertNotImplies(v, Equal(x, y))

  @Test def canBacktrack =
    assertImplies(v2, Equal(x, y)) // v2 is not empty
    assertImplies(True, or(and(v, v2), v3))
    assertImplies(v, Equal(x, y)) // v is not empty
