package dotty.tools.dotc.qualifiers

import dotty.tools.DottyTest

import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Types.{Type, AnnotatedType, ConstantType, MethodType, NamedType, NoPrefix, TermRef, ThisType, TypeRef}
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.Names.{Name, termName}
import dotty.tools.dotc.core.StdNames.nme

import dotty.tools.dotc.ast.tpd.{Tree, Apply, Ident, Lambda, Literal, New, Select}

import org.junit.Test
import org.junit.Assert.*

import QualifierExpr.*
import dotty.tools.dotc.core.Names.typeName

final class QualifierExprsTest extends DottyTest:
  def tp3 = ConstantType(Constant(3))
  def tp4 = ConstantType(Constant(4))
  def tp5 = ConstantType(Constant(5))

  val it = PredArg
  def qe3 = IntConst(3)
  def qe4 = IntConst(4)
  def qe5 = IntConst(5)

  @Test def `ofType(3) --> it == 3` =
    val expected = equal(it, qe3)
    val actual = QualifierExprs.fromType(tp3)
    assertEquals(expected, actual)

  @Test def `ofType(3 | 4) --> it == 3 or it == 4` =
    val expected = or(equal(it, qe3), equal(it, qe4))
    val actual = QualifierExprs.fromType(tp3 | tp4)
    assertEquals(expected, actual)

  // Example: AnnotatedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int),ConcreteAnnotation(Apply(TypeApply(Select(New(Select(Select(Select(Ident(_root_),scala),annotation),qualified)),<init>),List(Ident(Int))),List(Block(List(DefDef($anonfun,List(List(ValDef(v,Ident(Int),EmptyTree))),TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),Boolean)],Apply(Select(Ident(v),<=),List(Literal(Constant(0)))))),Closure(List(),Ident($anonfun),EmptyTree))))))
  // Actual: AnnotatedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int),ConcreteAnnotation(Apply(TypeApply(Select(New(TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class annotation)),class qualified)]),<init>),List(TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)])),List(Block(List(DefDef($anonfun,List(List(ValDef(v,TypeTree[TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int)],EmptyTree))),TypeTree[TypeRef(ThisType(TypeRef(NoPrefix,module class scala)),class Boolean)],Apply(Select(Ident(v),<=),List(Literal(Constant(3)))))),Closure(List(),Ident($anonfun),EmptyTree))))))

  def qualifiedType(tp: Type)(pred: Tree => Tree) =
    val predArg = termName("v")
    val qualifierMethodType = MethodType(List(predArg))(_ => List(tp), _ => defn.BooleanType)
    val lambda = Lambda(qualifierMethodType, args => pred(args.head))
    val annotation = Annotation(New(defn.QualifiedAnnot.termRef.appliedTo(tp), List(lambda)))
    AnnotatedType(tp, annotation)

  def intPredicate(op: Name, rhs: Int)(lhs: Tree): Tree =
    println(Select(lhs, op).tpe.asInstanceOf[TermRef].underlying.show)
    println("---")
    Apply(Select(lhs, op), List(Literal(Constant(rhs))))

  def intEquals(rhs: Int)(lhs: Tree) = intPredicate(nme.EQ, rhs)(lhs)
  def intNotEquals(rhs: Int)(lhs: Tree) = intPredicate(nme.NE, rhs)(lhs)
  def intLessThan(rhs: Int)(lhs: Tree) = intPredicate(nme.LE, rhs)(lhs)

  @Test def `ofType({v: Int with v == 3}) --> it == 3`: Unit =
    val intTp = TypeRef(TermRef(defn.RootClass.thisType, defn.ScalaPackageVal), defn.IntClass)
    println(intTp)
    val tp = qualifiedType(intTp)(intLessThan(3))
    println(tp.show)
    val expected = equal(it, qe3)
    val actual = QualifierExprs.fromType(tp)
    assertEquals(expected, actual)

  //@Test def `ofType({v: Int with v > 0}) --> not(it == 0) and not(it < 0)` = ???
