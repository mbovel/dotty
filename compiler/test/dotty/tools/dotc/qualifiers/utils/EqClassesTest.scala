package dotty.tools.dotc.qualifiers.utils

import org.junit.Test
import org.junit.Assert.*
import org.junit.FixMethodOrder
import org.junit.runners.MethodSorters

@FixMethodOrder(MethodSorters.JVM)
class EqClassesTest:
  val eqs = new EqClasses[Int]

  private def assertEquivalent(elems: Int*) =
    assertEquals(1, elems.map(eqs.repr).distinct.size)

  @Test def singleEquality =
    eqs.addEq(1, 2)
    assertEquivalent(1, 2)

  @Test def reflexiveByDefault =
    assertEquals(0, eqs.nClasses)
    assertEquivalent(1)
    assertEquals(0, eqs.nClasses)

  @Test def transitivity1 =
    eqs.addEq(1, 2)
    eqs.addEq(2, 3)
    assertEquals(1, eqs.nClasses)
    assertEquivalent(1, 2, 3)

  @Test def transitivity2 =
    eqs.addEq(2, 1)
    eqs.addEq(2, 3)
    assertEquals(1, eqs.nClasses)
    assertEquivalent(1, 2, 3)
