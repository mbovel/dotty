package dotty.tools
package dotc
package qualifiers

import org.junit.Test
import org.junit.Assert.*

import QualifierExpr.*

final class QualifierExprTest:
  val x = Var(1)
  val y = Var(2)
  val z = Var(3)

  @Test def andLeftTrue =
    assertEquals(x, And(Set(True, x)).normalized())

  @Test def andFlattened =
    assertEquals(And(Set(x, y, z)), And(Set(And(Set(x, y)), z)).normalized())

  @Test def sumOrdered =
    assertEquals(IntSum(0, List(x, y)).normalized(), IntSum(0, List(y, x)).normalized())

  @Test def sumGrouped =
    assertEquals(IntSum(1, List(IntProduct(2, List(x)))), IntSum(1, List(x, x)).normalized())

  @Test def equalOrdered =
    assertEquals(Equal(x, y), Equal(y, x).normalized())

  @Test def sumGroupedSingle =
    assertEquals(IntProduct(2, List(x)), IntSum(0, List(x, x)).normalized())

  @Test def sumConsts =
    assertEquals(IntConst(9), IntSum(2, List(IntConst(3), IntConst(4))).normalized())

  @Test def productConsts =
    assertEquals(IntConst(24), IntProduct(2, List(IntConst(3), IntConst(4))).normalized())
