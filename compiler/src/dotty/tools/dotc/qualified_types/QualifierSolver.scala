package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.{closureDef, singleton, Apply, Ident, Literal, Select, Tree, given}
import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol, Symbol}
import dotty.tools.dotc.core.Types.{TermRef}
import dotty.tools.dotc.transform.BetaReduce

import dotty.tools.dotc.reporting.trace
import dotty.tools.dotc.config.Printers

class QualifierSolver(using Context):
  private val litTrue = Literal(Constant(true))
  private val litFalse = Literal(Constant(false))

  val d = defn // Need a stable path to match on `defn` members

  def implies(tree1: Tree, tree2: Tree) =
    trace.force(i"implies $tree1 -> $tree2", Printers.qualifiedTypes):
      (tree1, tree2) match
        case (closureDef(defDef1), closureDef(defDef2)) =>
          val tree1ArgSym = defDef1.symbol.paramSymss.head.head
          val tree2ArgSym = defDef2.symbol.paramSymss.head.head
          val rhs = defDef1.rhs
          val lhs = defDef2.rhs
          if tree1ArgSym.info frozen_<:< tree2ArgSym.info then
            impliesRec1(addContext(rhs), lhs.subst(List(tree2ArgSym), List(tree1ArgSym)))
          else if tree2ArgSym.info frozen_<:< tree1ArgSym.info then
            impliesRec1(addContext(rhs.subst(List(tree1ArgSym), List(tree2ArgSym))), lhs)
          else
            false
        case _ =>
          throw IllegalArgumentException("Qualifiers must be closures")

  private def addContext(tree: Tree): Tree =
    var references = Set[TermRef]()
    tree.foreachSubTree: subTree =>
      subTree match
        case subTree: Ident =>
          subTree.tpe match
            case tp: TermRef =>
              references = references + tp
            case _ => ()
        case _ => ()

    var context = List[Tree]()
    for ref <- references do
      ref.underlying match
        case QualifiedType(_, closureDef(qualifierDef)) =>
          context = BetaReduce.reduceApplication(qualifierDef, List(List(tpd.singleton(ref)))).get :: context
        case _ => ()
    //println(context)
    tree

  private def impliesRec1(tree1: Tree, tree2: Tree): Boolean =
    // tree1 = lhs || rhs
    tree1 match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Boolean_|| =>
            return impliesRec1(lhs, tree2) && impliesRec1(rhs, tree2)
          case _ => ()
      case _ => ()

    val eqs = topLevelEqualities(tree1)
    if !eqs.isEmpty then
      val (rewrittenTree1, rewrittenTree2) = rewriteEquivalences(tree1, tree2, eqs)
      return impliesRec2(rewrittenTree1, rewrittenTree2)

    impliesRec2(tree1, tree2)

  private def impliesRec2(tree1: Tree, tree2: Tree): Boolean =
    // tree2 = lhs && rhs, or tree2 = lhs || rhs
    tree2 match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Boolean_&& =>
            return impliesRec2(tree1, lhs) && impliesRec2(tree1, rhs)
          case d.Boolean_|| =>
            return impliesRec2(tree1, lhs) || impliesRec2(tree1, rhs)
          case _ => ()
      case _ => ()

    // tree1 = lhs && rhs
    tree1 match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Boolean_&& =>
            return impliesRec2(lhs, tree2) || impliesRec2(rhs, tree2)
          case _ => ()
      case _ => ()

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
    trace(i"topLevelEqualities $tree", Printers.qualifiedTypes):
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
    trace(i"rewriteEquivalences $tree1, $tree2, $eqs", Printers.qualifiedTypes):
      val egraph = QualifierEGraph()
      for (lhs, rhs) <- eqs do
        egraph.union(lhs, rhs)
      egraph.repair()
      (egraph.rewrite(tree1), egraph.rewrite(tree2))
