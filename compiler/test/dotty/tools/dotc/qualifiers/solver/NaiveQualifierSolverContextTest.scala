package dotty.tools.dotc.qualifiers
package solver

import collection.mutable

import org.junit.Test
import org.junit.Assert.*
import org.junit.FixMethodOrder
import org.junit.runners.MethodSorters

import QualifierExpr.*
import dotty.tools.dotc.qualifiers.solver.NaiveQualifierSolverContext.*

@FixMethodOrder(MethodSorters.JVM)
final class NaiveQualifierSolverContextTest:
  var ctx = NaiveQualifierSolverContext()

  val x = Ref(0, "x")
  val y = Ref(1, "y")
  val z = Ref(2, "z")
  val z2 = Ref(3, "z2")
  val z3 = Ref(4, "z3")
  val z4 = Ref(5, "z3")
  val fRef = Ref(6, "f")
  def f(e: QualifierExpr) = App(fRef, List(e))
  val gRef = Ref(7, "g")
  def g(e: QualifierExpr) = App(gRef, List(e))

  private def assertEquivalent(elems: QualifierExpr*) =
    assertEquals(1, elems.map(ctx.rewrite).distinct.size)

  private def addEq(x: QualifierExpr, y: QualifierExpr) =
    ctx = ctx.assume(Equal(x, y))

  @Test def singleEquality =
    addEq(x, y)
    assertEquivalent(x, y)

  @Test def reflexiveByDefault =
    assertEquals(0, ctx.nClasses())
    assertEquivalent(x, x)
    // The above should not change the EqClassMap
    assertEquals(0, ctx.nClasses())

  @Test def transitivity1 =
    addEq(x, y)
    addEq(y, z)
    assertEquals(1, ctx.nClasses())
    assertEquivalent(x, y, z)

  @Test def transitivity2 =
    addEq(y, x)
    addEq(y, z)
    assertEquals(1, ctx.nClasses())
    assertEquivalent(x, y, z)

  @Test def nested1 =
    addEq(f(x), z)
    addEq(f(y), z2)
    addEq(z, z2)
    assertEquals(1, ctx.nClasses())
    assertEquivalent(f(x), f(y), z, z2)

  @Test def nested2 =
    addEq(f(x), z)
    addEq(f(y), z2)
    addEq(g(f(x)), z3)
    addEq(z, z2)
    assertEquals(2, ctx.nClasses())
    assertEquivalent(f(x), f(y), z, z2)
    assertEquivalent(g(f(y)), g(f(x)), z3)

  @Test def nested3 =
    addEq(x, y)
    addEq(f(x), z)
    assertEquals(2, ctx.nClasses())
    assertEquivalent(x, y)
    assertEquivalent(f(x), z)
    assertEquivalent(f(y), z)

  @Test def nested4 =
    addEq(x, y)
    addEq(f(x), z)
    assertEquals(2, ctx.nClasses())
    assertEquivalent(x, y)
    assertEquivalent(f(x), z)
    assertEquivalent(g(f(y)), g(z))

