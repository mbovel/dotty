package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.ast.tpd.{Apply,Ident, Literal, singleton, closureDef, Select, Tree, given}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.{defn, Symbol, NoSymbol}
import dotty.tools.dotc.core.Constants.Constant
import QualifierTracing.trace
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.config.Printers

class QualifierSolver(using Context):
  private val litTrue = Literal(Constant(true))
  private val litFalse = Literal(Constant(false))

  def implies(tree1: Tree, tree2: Tree) =
    (tree1, tree2) match
      case (closureDef(defDef1), closureDef(defDef2)) =>
        val tree1ArgSym = defDef1.symbol.paramSymss.head.head
        val tree2ArgSym = defDef2.symbol.paramSymss.head.head
        impliesRec(defDef1.rhs, defDef2.rhs, tree1ArgSym, tree2ArgSym)
      case _ =>
        throw IllegalArgumentException("Qualifiers must be closures")

  def isTrue(tree: Tree): Boolean =
    tree match
      case closureDef(defDef) =>
        val argSym = defDef.symbol.paramSymss.head.head
        QualifierAlphaComparer().same(QualifierEvaluator.evaluate(defDef.rhs), litTrue)
      case _ =>
        throw IllegalArgumentException("Qualifier must be a closure")

  private def impliesRec(tree1: Tree, tree2: Tree, tree1ArgSym: Symbol, tree2ArgSym: Symbol): Boolean =
    val d = defn // Need a stable path to match on `defn` members
    tree1 match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Boolean_|| =>
            return impliesRec(lhs, tree2, tree1ArgSym, tree2ArgSym) && impliesRec(rhs, tree2, tree1ArgSym, tree2ArgSym)
          case _ => ()
      case _ => ()

    tree2 match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Boolean_&& =>
            return impliesRec(tree1, lhs, tree1ArgSym, tree2ArgSym) && impliesRec(tree1, rhs, tree1ArgSym, tree2ArgSym)
          case d.Boolean_|| =>
            return impliesRec(tree1, lhs, tree1ArgSym, tree2ArgSym) || impliesRec(tree1, rhs, tree1ArgSym, tree2ArgSym)
          case _ => ()
      case _ => ()

    val equivs = equalTo(tree1, tree1ArgSym)
    if
      equivs.exists: tree1Arg =>
        impliesLeaf(
          QualifierEvaluator.evaluate(tree1, tree1ArgSym, tree1Arg),
          QualifierEvaluator.evaluate(tree2, tree2ArgSym, tree1Arg)
        )
    then
      return true

    impliesLeaf(
      QualifierEvaluator.evaluate(tree1),
      QualifierEvaluator.evaluate(tree2, tree2ArgSym, Ident(tree1ArgSym.termRef)),
    )


  private def impliesLeaf(tree1: Tree, tree2: Tree): Boolean =
    trace(i"impliesLeaf $tree1 -> $tree2"):
      impliesLeafImpl(tree1, tree2)

  private def impliesLeafImpl(tree1: Tree, tree2: Tree): Boolean =
    tree2 match
      case Literal(Constant(true)) =>
        return true
      case _ => ()

    tree1 match
      case Literal(Constant(false)) =>
        return true
      case _ => ()

    QualifierAlphaComparer().same(tree1, tree2)

  def equalTo(qualifier: Tree, argSym: Symbol): List[Tree] =
    val d = defn
    qualifier match
      case Apply(select @ Select(lhs, name), List(rhs)) =>
        select.symbol match
          case d.Boolean_&& =>
            equalTo(lhs, argSym) ++ equalTo(rhs, argSym)
          case d.Boolean_== | d.Int_== | d.Any_== =>
            if lhs.symbol == argSym then List(rhs)
            else if rhs.symbol == argSym then List(lhs)
            else Nil
          case _ =>
            Nil
      case _ =>
        Nil
