package dotty.tools.dotc.qualifiers
package solver

import collection.mutable

import org.junit.Test
import org.junit.Assert.*

import QualifierExpr.*

final class NaiveQualifierSolverTest extends QualifierSolverTest(NaiveQualifierSolver()):
  @Test def `it == 5 can imply 5 == it` =
    assertImplies(Equal(IntConst(5), PredArg), Equal(PredArg, IntConst(5)))

  @Test def `it == 5 cannot imply it == 6` =
    assertNotImplies(Equal(PredArg, IntConst(5)), Equal(PredArg, IntConst(6)))

  @Test def `it == 5 can imply it < 6` =
    assertImplies(Equal(PredArg, IntConst(5)), LessThan(PredArg, IntConst(6)))

  @Test def `p or q can imply r if p implies and q implies r` =
    val p = Equal(PredArg, IntConst(5))
    val q = Equal(IntConst(5), PredArg)
    val r = Equal(IntConst(5), IntConst(5))
    assertImplies(Or(List(p, q)), r)

  @Test def `p or q cannot imply r if q cannot imply r` =
    val p = Equal(PredArg, IntConst(5))
    val q = Equal(PredArg, IntConst(6))
    val r = Equal(PredArg, IntConst(5))
    assertNotImplies(Or(List(p, q)), r)

  @Test def `p and q can imply r if p implies or q implies r` =
    val p = Equal(PredArg, IntConst(5))
    val q = Equal(PredArg, IntConst(6))
    val r = Equal(PredArg, IntConst(5))
    assertImplies(And(List(p, q)), r)

  @Test def `p and q cannot imply r if nor p nor q can imply r` =
    val p = Equal(PredArg, IntConst(5))
    val q = Equal(PredArg, IntConst(6))
    val r = Equal(PredArg, IntConst(7))
    assertNotImplies(And(List(p, q)), r)

  @Test def `p and q can imply p and q` =
    val p = Equal(PredArg, IntConst(5))
    val q = Equal(PredArg, IntConst(6))
    assertImplies(And(List(p, q)), And(List(p, q)))

  @Test def `p or q can imply p or q` =
    val p = Equal(PredArg, IntConst(5))
    val q = Equal(PredArg, IntConst(6))
    assertImplies(Or(List(p, q)), Or(List(p, q)))

  @Test def `given p implies v, v cannot imply q` =
    val p = Equal(PredArg, IntConst(5))
    val q = Equal(PredArg, IntConst(6))
    assertImplies(p, v)
    assertNotImplies(v, q)

  @Test def `given v implies q, p cannot imply v` =
    val p = Equal(PredArg, IntConst(5))
    val q = Equal(PredArg, IntConst(6))
    assertImplies(v, q)
    assertNotImplies(p, v)


  @Test def baseline =
    assertEquals(4, 2 + 2)

  /*----------*/
  /* Equality */
  /*----------*/

  @Test def transitiveEquality() =
    val eq = and(Equal(x, y), Equal(y, z))
    assertImplies(eq, Equal(x, z))
