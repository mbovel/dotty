package dotty.tools.dotc
package qualifiers

import transform.MegaPhase.MiniPhase
import ast.{TreeTypeMap, tpd}
import core.*
import Contexts.*, Symbols.*, Types.*, Constants.*, StdNames.*, Decorators.*
import config.Printers.overload
import core.Symbols.defn
import core.Decorators.*
import tpd.*
import dotty.tools.dotc.reporting.trace

import ast.untpd
import transform.Erasure.Boxing.*
import TypeErasure.*

import core.Flags.*
import util.Spans.*
import reporting.*
import config.Printers.{transforms as debug}

import dotty.tools.dotc.util.SrcPos

import ast.tpd.*
import typer.Inferencing.maximizeType
import typer.ProtoTypes.constrained
import dotty.tools.dotc.cc.CaptureSet.empty
import typer.*

import transform.Recheck.knownType
import qualifiers.QualifierExprs
import scala.annotation.switch

class QualifiedTypesTypeTests extends MiniPhase:

  override def phaseName: String = QualifiedTypesTypeTests.name

  override def description: String = QualifiedTypesTypeTests.description

  override def transformTypeApply(tree: TypeApply)(using Context): tpd.Tree =
    if tree.symbol.isTypeTest then
      val expr = tree.fun match
        case Select(expr, _) => expr
        case i: Ident        =>
          // Copied from TypeTestCases
          // Do we need this ?
          // val expr = desugarIdentPrefix(i)
          // if (expr.isEmpty) expr
          // else expr.withSpan(i.span)
          throw new Exception("Unreachable code")
        case _ => tree
      val testType = tree.args.head.knownType
      transformTypeTest(expr, testType)
    else
      tree

  def transformTypeTest(expr: Tree, testType: Type)(using Context): Tree =
    val newTree =
      testType match
        case AndType(tp1, tp2) if containsQualifiedTypes(tp1) || containsQualifiedTypes(tp2) =>
          val expr1 = transformTypeTest(expr, tp1)
          val expr2 = transformTypeTest(expr, tp2)
          expr1.and(expr2)

        case OrType(tp1, tp2) if containsQualifiedTypes(tp1) || containsQualifiedTypes(tp2) =>
          val expr1 = transformTypeTest(expr, tp1)
          val expr2 = transformTypeTest(expr, tp2)
          expr1.or(expr2)

        case EventuallyQualifiedType(baseType, qualifier) =>
          val exprCast = expr.asInstance(baseType)
          val appliedQualifier = QualifierExprs.toTree(qualifier, exprCast, baseType)
          transformTypeTest(expr, baseType).and(appliedQualifier)

        case _ =>
          expr.isInstance(testType)

    newTree

  def containsQualifiedTypes(tp: Type)(using Context): Boolean =
    def isQualified(tp: Type) =
      tp match
        case EventuallyQualifiedType(_, _) => true
        case _                             => false

    ExistsAccumulator(isQualified, StopAt.Package, forceLazy = true)(false, tp)

object QualifiedTypesTypeTests:
  val name: String = "QualifiedTypesTypeTests"
  val description: String = ""
