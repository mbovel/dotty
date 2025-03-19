package dotty.tools.dotc.qualified_types

import scala.collection.mutable.ListBuffer
import scala.util.boundary
import scala.util.boundary.break

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.{Apply, EmptyTree, Ident, Lambda, Literal, Select, Tree, given}
import dotty.tools.dotc.core.Atoms
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.{i, toTermName}
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{defn, Symbol}
import dotty.tools.dotc.core.Types.{AndType, MethodType, OrType, Type, TypeProxy}
import dotty.tools.dotc.qualified_types.QualifierTracing.trace
import dotty.tools.dotc.transform.BetaReduce

object QualifiedTypes:
  private val constTrue = Constant(true)
  private val constFalse = Constant(false)

  /** Does the type `tp1` imply the qualifier `qualifier2`?
   *
   *  Used by [[dotty.tools.dotc.core.TypeComparer]] to compare qualified types.
   *
   *  Note: the logic here is similar to [[Type#derivesAnnotWith]] but
   *  additionally handle comparisons with [[SingletonType]]s.
   */
  def typeImplies(tp1: Type, qualifier2: Tree)(using Context): Boolean =
    trace(i"typeImplies $tp1 $qualifier2"):
      tp1 match
        case QualifiedType(parent1, qualifier1) =>
          QualifierSolver().implies(qualifier1, qualifier2)
        case SingleAtom(tp1) =>
          QualifierAlphaComparer().same(
            QualifierEvaluator.applyQualifierTo(qualifier2, tpd.singleton(tp1)),
            Literal(constTrue)
          )
        case tp1: TypeProxy =>
          typeImplies(tp1.underlying, qualifier2)
        case AndType(tp11, tp12) =>
          typeImplies(tp11, qualifier2) || typeImplies(tp12, qualifier2)
        case OrType(tp11, tp12) =>
          typeImplies(tp11, qualifier2) && typeImplies(tp12, qualifier2)
        case _ =>
          println(i"tp1: $tp1")
          println(i"qualifier2: $qualifier2")
          QualifierSolver().isTrue(qualifier2)

  /** Try to adapt the tree to the given type `pt`
   *
   *  Returns [[EmptyTree]] if `pt` does not contain qualifiers or if the tree
   *  cannot be adapted, or the adapted tree otherwise.
   *
   *  Used by [[dotty.tools.dotc.core.Typer]].
   */
  def adapt(tree: Tree, pt: Type)(using Context): Tree =
    if containsQualifier(pt) && QualifierEvaluator.isSimple(tree) then
      trace(i"adapt $tree to $pt"):
        val selfifiedTp = QualifiedType(tree.tpe, selfify(tree))
        if selfifiedTp <:< pt then tree.withType(selfifiedTp) else EmptyTree
    else
      EmptyTree

  private def containsQualifier(tp: Type)(using Context): Boolean =
    tp match
      case QualifiedType(_, _) => true
      case tp: TypeProxy       => containsQualifier(tp.underlying)
      case AndType(tp1, tp2)   => containsQualifier(tp1) || containsQualifier(tp2)
      case OrType(tp1, tp2)    => containsQualifier(tp1) || containsQualifier(tp2)
      case _                   => false

  private def selfify(tree: Tree)(using Context): Tree =
    Lambda(
      MethodType(List("v".toTermName))(_ => List(tree.tpe), _ => defn.BooleanType),
      (args) =>
        val argRef = Ident(args(0).symbol.termRef)
        argRef.equal(tree)
    )
