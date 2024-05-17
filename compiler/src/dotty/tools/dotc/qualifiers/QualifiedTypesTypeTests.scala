package dotty.tools.dotc
package transform

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
import Erasure.Boxing.*
import TypeErasure.*

import core.Flags.*
import util.Spans.*
import reporting.*
import config.Printers.{ transforms => debug }

import patmat.Typ
import dotty.tools.dotc.util.SrcPos

import ast.tpd.*
import typer.Inferencing.maximizeType
import typer.ProtoTypes.constrained
import dotty.tools.dotc.cc.CaptureSet.empty
import typer.*

import Recheck.knownType
import qualifiers.QualifierExprs


class QualifiedTypesTypeTests extends MiniPhase{

  override def phaseName: String = QualifiedTypesTypeTests.name

  override def description: String = QualifiedTypesTypeTests.description

  override def transformTypeApply(tree: TypeApply)(using Context): tpd.Tree =
    println("************ transformTypeApply ************")
    if tree.symbol.isTypeTest then
      val expr = tree.fun match
        case Select(expr, _) => expr
        case i: Ident =>
          // Copied from TypeTestCases
          // Do we need this ?
          // val expr = desugarIdentPrefix(i)
          // if (expr.isEmpty) expr
          // else expr.withSpan(i.span)
          throw new Exception("Unreachable code")
        case _ => tree
      val testType = tree.args.head.knownType
      println(i"expr: $expr, testType: $testType")

      transformTypeTest(expr, testType)
    else
      tree

  def transformTypeTest(expr: Tree, testType: Type)(using Context): Tree =
    val newTree =
      testType match
        case AndType(tp1, tp2) if containsQualifiedTypes(tp1) || containsQualifiedTypes(tp2) =>
          throw new Exception("TODO")
        // TODO Add OrType case
        case qualifiers.QualifiedType(baseType, qualifier) =>
          println("qualifier: " + qualifier)
          val exprCast = expr.asInstance(baseType)
          val appliedQualifier = QualifierExprs.toTree(qualifier, exprCast, baseType)
          println("appliedQualifier: " + appliedQualifier)
          transformTypeTest(expr, baseType).and(appliedQualifier)
        case _ =>
          expr.isInstance(testType)

    println(i"new tree: $newTree")
    newTree


  def containsQualifiedTypes(tp: Type)(using Context): Boolean =
    // Use an ExistsAccumulator to check if the type contains QualifiedTypes
    true
}



object QualifiedTypesTypeTests:
  val name: String = "QualifiedTypesTypeTests"
  val description: String = ""

