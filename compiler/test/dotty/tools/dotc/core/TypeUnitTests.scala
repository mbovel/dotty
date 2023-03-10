package dotty.tools
package dotc
package core

import Contexts.{Context, NoContext}
import Flags.EmptyFlags
import Names.{TypeName, typeName}
import Symbols.{TypeSymbol, NoSymbol, defn, newSymbol}
import Types.{Type, AppliedType, HKTypeLambda, TypeBounds, SkolemType, TypeRef, NoPrefix, NoType}
import Variances.Invariant

import org.junit.Test
import org.junit.Assert.{assertEquals}

class TypeUnitTests extends DottyTest:

  @Test def appliedToSkolem =
    val T: TypeName = typeName("T")
    /*val S: TypeName = typeName("S")
    val F2 = TypeRef(
      NoPrefix,
      newSymbol(
        defn.RootClass,
        typeName("F2"),
        Flags.Opaque,
        HKTypeLambda(List(T, S), List(Invariant, Invariant))(
          _ => List(TypeBounds.empty, TypeBounds.empty),
          self => self.paramRefs(0)
        )
      )
    )
    val F2 = TypeRef(NoPrefix, NoSymbol)*/
    val Id = HKTypeLambda(List(T), List(Invariant))(
      _ => List(TypeBounds.empty),
      self => self.paramRefs(0)
    )
    //val Skolem = SkolemType(defn.IntType)
    assertTypesEquiv(
      Id.appliedTo(TypeBounds(defn.NothingType, defn.IntType)),
      TypeBounds(defn.NothingType, defn.IntType)
    )

  def assertTypesEquiv(a: Type, b: Type) =
    if !(a =:= b) then
      throw new AssertionError(f"${a.show} is not equivalent to ${b.show}")
