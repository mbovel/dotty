package dotty.tools.dotc.qualifiers

import org.junit.Test
import org.junit.Assert.*

import QualifierExpr.*

final class QualifierExprTest:
  val x = Var(1)
  val y = Var(2)
  val z = Var(3)

  @Test def `andLeftTrue` =
    assertEquals(x, And(List(True, x)).simplify())

  @Test def `andFlattened` =
    assertEquals(And(List(x, y, z)), And(List(And(List(x, y)), z)).simplify())

  @Test def `sumOrdered` =
    assertEquals(IntSum(0, List(x, y)).simplify(), IntSum(0, List(y, x)).simplify())

  @Test def `sumGrouped` =
    assertEquals(IntSum(1, List(IntProduct(2, List(x)))), IntSum(1, List(x, x)).simplify())

  @Test def `equalOrdered` =
    assertEquals(Equal(x, y), Equal(y, x).simplify())

  @Test def `sumGroupedSingle` =
    assertEquals(IntProduct(2, List(x)), IntSum(0, List(x, x)).simplify())

  @Test def `sumConsts` =
    assertEquals(IntConst(9), IntSum(2, List(IntConst(3), IntConst(4))).simplify())

  @Test def `productConsts` =
    assertEquals(IntConst(24), IntProduct(2, List(IntConst(3), IntConst(4))).simplify())
