package dotty.tools.dotc.qualifiers

import org.junit.Test
import org.junit.Assert.*

import QualifierExpr.*
import dotty.tools.io.Path.extension

final class QualifierExprTest:
  val x = Ref(0, "x")
  val y = Ref(1, "y")
  val z = Ref(2, "z")

  /*-----------------------*/
  /* Constructor functions */
  /*------------------ ----*/

  @Test def intSumPreservesOrder =
    assertEquals(intSum(y, x), IntSum(0, List(y, x)))

  @Test def intProductPreservesOrder =
    assertEquals(intProduct(y, x), IntProduct(1, List(y, x)))

  /*---------*/
  /* Mapping */
  /*---------*/

  @Test def andLeftTrue =
    assertEquals(x, And(List(True, x)).map(x => x))

  @Test def andFlattened =
    assertEquals(And(List(x, y, z)), And(List(And(List(x, y)), z)).map(x => x))

  @Test def sumConsts =
    assertEquals(IntConst(9), IntSum(2, List(IntConst(3), IntConst(4))).map(x => x))

  @Test def productConsts =
    assertEquals(IntConst(24), IntProduct(2, List(IntConst(3), IntConst(4))).map(x => x))

  /*-----------*/
  /* Normalize */
  /*-----------*/

  @Test def equalOrdered =
    assertEquals(Equal(x, y), Equal(y, x).shallowNormalize())

  @Test def lessThanOrdered =
    assertEquals(LessThan(x, y), LessThan(y, x).shallowNormalize())

  @Test def sumOrdered =
    assertEquals(IntSum(0, List(x, y)).shallowNormalize(), IntSum(0, List(y, x)).shallowNormalize())

  @Test def sumGrouped =
    assertEquals(IntSum(1, List(IntProduct(2, List(x)))), IntSum(1, List(x, x)).shallowNormalize())

  @Test def sumGroupedSingle =
    assertEquals(IntProduct(2, List(x)), IntSum(0, List(x, x)).shallowNormalize())
