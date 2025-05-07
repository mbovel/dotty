package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.tpd.{closureDef, singleton, Apply, Ident, Literal, Select, Tree, given}
import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol, Symbol}

import QualifierTracing.trace

class QualifierSolver(using Context):
  private val litTrue = Literal(Constant(true))
  private val litFalse = Literal(Constant(false))

  def implies(tree1: Tree, tree2: Tree) =
    trace(i"implies $tree1 -> $tree2"):
      (tree1, tree2) match
        case (closureDef(defDef1), closureDef(defDef2)) =>
          val tree1ArgSym = defDef1.symbol.paramSymss.head.head
          val tree2ArgSym = defDef2.symbol.paramSymss.head.head
          val leftMoreSpecific = tree1ArgSym.info frozen_<:< tree2ArgSym.info
          val rhs1 = if leftMoreSpecific then defDef1.rhs else defDef1.rhs.subst(List(tree1ArgSym), List(tree2ArgSym))
          val rhs2 = if leftMoreSpecific then defDef2.rhs.subst(List(tree2ArgSym), List(tree1ArgSym)) else defDef2.rhs
          impliesRec(rhs1, rhs2)
        case _ =>
          throw IllegalArgumentException("Qualifiers must be closures")

  private def impliesRec(tree1: Tree, tree2: Tree): Boolean =
    val d = defn // Need a stable path to match on `defn` members

    // tree1 = lhs || rhs
    tree1 match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Boolean_|| =>
            return impliesRec(lhs, tree2) && impliesRec(rhs, tree2)
          case _ => ()
      case _ => ()

    // tree2 = lhs && rhs, or tree2 = lhs || rhs
    tree2 match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Boolean_&& =>
            return impliesRec(tree1, lhs) && impliesRec(tree1, rhs)
          case d.Boolean_|| =>
            return impliesRec(tree1, lhs) || impliesRec(tree1, rhs)
          case _ => ()
      case _ => ()

    // tree1 = lhs && rhs
    tree1 match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Boolean_&& =>
            return impliesRec(lhs, tree2) || impliesRec(rhs, tree2)
          case _ => ()
      case _ => ()


    val eqs = topLevelEqualities(tree1)
    val tree1Rewritten = if !eqs.isEmpty then
      val (newTree1, newTree2) = rewriteEquivalences(tree1, tree2, eqs)
      return impliesRec(newTree1, newTree2)

    val tree1Normalized = QualifierNormalizer.normalize(QualifierEvaluator.evaluate(tree1))
    val tree2Normalized = QualifierNormalizer.normalize(QualifierEvaluator.evaluate(tree2))

    tree2Normalized match
      case Literal(Constant(true)) =>
        return true
      case _ => ()

    tree1Normalized match
      case Literal(Constant(false)) =>
        return true
      case _ => ()

    QualifierAlphaComparer().iso(tree1Normalized, tree2Normalized)

  private def topLevelEqualities(tree: Tree): List[(Tree, Tree)] =
    trace(i"topLevelEqualities $tree"):
      topLevelEqualitiesImpl(tree)

  private def topLevelEqualitiesImpl(tree: Tree): List[(Tree, Tree)] =
    val d = defn
    tree match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Int_== | d.Any_== | d.Boolean_== => List((lhs, rhs))
          case d.Boolean_&& => topLevelEqualitiesImpl(lhs) ++ topLevelEqualitiesImpl(rhs)
          case _ => Nil
      case _ =>
        Nil

  private def rewriteEquivalences(tree1: Tree, tree2: Tree, eqs: List[(Tree, Tree)]): (Tree, Tree) =
    trace(i"rewriteEquivalences $tree1, $tree2, $eqs"):
      val egraph = QualifierEGraph()
      for (lhs, rhs) <- eqs do
        egraph.union(lhs, rhs)
      (egraph.rewrite(tree1), egraph.rewrite(tree2))
