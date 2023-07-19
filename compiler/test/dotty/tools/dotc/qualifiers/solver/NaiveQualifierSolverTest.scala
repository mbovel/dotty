package dotty.tools
package dotc
package qualifiers
package solver

import org.junit.Test
import org.junit.Assert.*

import QualifierExpr.*
import org.junit.runner.Description

final class NaiveQualifierSolverTest extends QualifierSolverTest(NaiveQualifierSolver()):
  @Test def `it == 5 can imply 5 == it` =
    assertTrue(Equal(IntConst(5), predArg).tryImply(Equal(predArg, IntConst(5))))

  @Test def `it == 5 cannot imply it == 6` =
    assertFalse(Equal(predArg, IntConst(5)).tryImply(Equal(predArg, IntConst(6))))

  @Test def `it == 5 cannot imply it < 6 (current limitation)` =
    assertFalse(Equal(predArg, IntConst(5)).tryImply(LessThan(predArg, IntConst(6))))

  @Test def `p or q can imply r if p implies and q implies r` =
    val p = Equal(predArg, IntConst(5))
    val q = Equal(IntConst(5), predArg)
    val r = Equal(IntConst(5), IntConst(5))
    assertTrue(Or(Set(p, q)).tryImply(r))

  @Test def `p or q cannot imply r if q cannot imply r ` =
    val p = Equal(predArg, IntConst(5))
    val q = Equal(predArg, IntConst(6))
    val r = Equal(predArg, IntConst(5))
    assertFalse(Or(Set(p, q)).tryImply(r))

  @Test def `p and q can imply r if p implies or q implies r` =
    val p = Equal(predArg, IntConst(5))
    val q = Equal(predArg, IntConst(6))
    val r = Equal(predArg, IntConst(5))
    assertTrue(And(Set(p, q)).tryImply(r))

  @Test def `p and q cannot imply r if nor p nor q can imply r ` =
    val p = Equal(predArg, IntConst(5))
    val q = Equal(predArg, IntConst(6))
    val r = Equal(predArg, IntConst(7))
    assertFalse(And(Set(p, q)).tryImply(r))
