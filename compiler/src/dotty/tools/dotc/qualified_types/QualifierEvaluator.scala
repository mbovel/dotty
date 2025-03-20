package dotty.tools.dotc.qualified_types

import scala.annotation.tailrec

import dotty.tools.dotc.ast.tpd.{
  allArguments,
  closureDef,
  funPart,
  isIdempotentExpr,
  singleton,
  Apply,
  Bind,
  Block,
  CaseDef,
  DefDef,
  EmptyTree,
  Ident,
  Literal,
  Match,
  New,
  Select,
  SeqLiteral,
  Tree,
  TreeMap,
  TypeApply,
  Typed,
  UnApply,
  ValDef,
  given
}
import dotty.tools.dotc.core.Atoms
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Mode.Type
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.{defn, NoSymbol, Symbol}
import dotty.tools.dotc.core.SymDenotations.given
import dotty.tools.dotc.core.Types.{ConstantType, NoPrefix}
import dotty.tools.dotc.transform.TreeExtractors.BinaryOp
import dotty.tools.dotc.transform.patmat.{Empty as EmptySpace, SpaceEngine}
import dotty.tools.dotc.inlines.InlineReducer
import dotty.tools.dotc.typer.Typer

import QualifierTracing.trace

private[qualified_types] object QualifierEvaluator:
  /** Reduces a tree by constant folding, simplification and unfolding of simple
   *  references and method applications.
   *
   *  This is more aggressive than [[dotty.tools.dotc.transform.BetaReduce]] and
   *  [[dotty.tools.dotc.typer.ConstFold]] (which is used under the hood by
   *  `BetaReduce` through [[dotty.tools.dotc.ast.tpd.cpy]]), as it also unfolds
   *  some non-constant expressions.
   */
  def evaluate(tree: Tree, argSym: Symbol = NoSymbol, argTree: Tree = EmptyTree)(using Context): Tree =
    trace(i"evaluate $tree with arg $argSym = $argTree"):
      val args = if argSym.exists then Map(argSym -> argTree) else Map.empty
      QualifierEvaluator(args).transform(tree)

  /** Applies the qualifier `qualifier` to the argument `arg`.
   *
   *  Pre-condition: `qualifier` is a closure.
   *
   *  Note: uses [[reduce]].
   */
  def applyQualifierTo(qualifier: Tree, arg: Tree)(using Context): Tree =
    qualifier match
      case closureDef(defDef) =>
        val argSym = defDef.symbol.paramSymss.head.head
        // TODO(mbovel): check if the argument has the right type?
        evaluate(defDef.rhs, argSym, arg)
      case _ =>
        throw IllegalArgumentException("Qualifier is not a closure")

  def isSimple(tree: Tree)(using Context): Boolean =
    tree match
      case _: (Literal | Ident) => isIdempotentExpr(tree) && !tree.tpe.isRef(defn.UnitClass)
      case Select(qual, _)      => isSimple(qual)
      case Apply(fn, args)      => isSimple(fn) && args.forall(isSimple)
      case TypeApply(fn, args)  => isSimple(fn)
      case SeqLiteral(elems, _) => elems.forall(isSimple)
      case Typed(expr, _)       => isSimple(expr)
      case Block(Nil, expr)     => isSimple(expr)
      case _                    => false

private class QualifierEvaluator(var args: Map[Symbol, Tree] = Map.empty, var unfoldCalls: Boolean = true) extends TreeMap:
  import QualifierEvaluator.*

  def recur(tree: Tree, args: Map[Symbol, Tree] = args, unfoldCalls: Boolean = unfoldCalls)(using Context): Tree =
    val prevArgs = this.args
    val prevUnfoldCalls = this.unfoldCalls
    this.args = args
    this.unfoldCalls = unfoldCalls
    val res = transform(tree)
    this.args = prevArgs
    this.unfoldCalls = prevUnfoldCalls
    res

  override def transform(tree: Tree)(using Context): Tree =
    unfold(reduce(tree))

  private def reduce(tree: Tree)(using Context): Tree =
    tree match
      case tree: Apply =>
        val treeTransformed = super.transform(tree)
        constFold(treeTransformed)
          .orElse(reduceBinaryOp(treeTransformed))
          .orElse(treeTransformed)
      case tree: Select =>
        val treeTransformed = super.transform(tree)
        constFold(treeTransformed)
          .orElse(treeTransformed)
      case Match(selector, cases) =>
        val selectorTransformed = transform(selector)
        val res = InlineReducer(tree.span).reduceInlineMatch(selectorTransformed, selectorTransformed.tpe, cases, new Typer(0))
        val res2 = reduceCases(selectorTransformed, cases)
        println(i"res = $res,\n res2 = $res2")

        res2.orElse(cpy.Match(tree)(selectorTransformed, cases.map(recur(_, args, false).asInstanceOf[CaseDef])))
      case Block(Nil, expr) =>
        transform(expr)
      case tree =>
        super.transform(tree)

  private def constFold(tree: Tree)(using Context): Tree =
    tree.tpe match
      case tp: ConstantType => singleton(tp)
      case _                => EmptyTree

  private def reduceBinaryOp(tree: Tree)(using Context): Tree =
    val d = defn // Need a stable path to match on `defn` members
    tree match
      case BinaryOp(a, d.Int_== | d.Any_== | d.Boolean_==, b) =>
        val aNormalized = QualifierNormalizer.normalize(a)
        val bNormalized = QualifierNormalizer.normalize(b)
        if QualifierAlphaComparer().iso(aNormalized, bNormalized) then
          Literal(Constant(true))
        else
          EmptyTree
      case _ =>
        EmptyTree

  private def reduceCases(selector: Tree, cases: List[CaseDef])(using Context): Tree =
    cases match
      case Nil => EmptyTree
      case CaseDef(pattern, guard, tree) :: rest =>
        matches(selector, pattern) match
          case MatchResult.Matched(bindings) =>
            recur(tree, args ++ bindings)
          case MatchResult.Disjoint =>
            reduceCases(selector, rest)
          case MatchResult.Unknown =>
            EmptyTree

  enum MatchResult:
    case Matched(bindings: Map[Symbol, Tree])
    case Disjoint
    case Unknown

    infix def and(other: MatchResult): MatchResult =
      (this, other) match
        case (Matched(bindings1), Matched(bindings2)) =>
          Matched(bindings1 ++ bindings2)
        case (Disjoint, _) | (_, Disjoint) =>
          Disjoint
        case _ =>
          Unknown

    infix def or(other: MatchResult): MatchResult =
      (this, other) match
        case (Matched(_), _) | (_, Matched(_)) =>
          Matched(Map.empty)
        case (Disjoint, Disjoint) =>
          Disjoint
        case _ =>
          Unknown

    def shortName(): String =
      this match
        case Matched(_) => "Matched"
        case Disjoint   => "Disjoint"
        case Unknown    => "Unknown"

  private def matches(selector: Tree, pattern: Tree, bindings: Map[Symbol, Tree] = Map.empty)(using Context) =
    trace[MatchResult](i"match $selector with $pattern", _.shortName()):
      matchesRec(selector, pattern, bindings)

  private def matchesRec(selector: Tree, pattern: Tree, bindings: Map[Symbol, Tree])(using Context): MatchResult =
    pattern match
      case Bind(name, pat) =>
        matchesRec(selector, pat, bindings.updated(pattern.symbol, selector))

      case UnApply(fun, _, pats) =>
        val sym = funPart(fun).symbol
        val module = sym.owner.denot.companionModule
        val clazz = sym.owner.denot.companionClass
        if sym.name == nme.unapply && clazz.is(Flags.Case) && !hasCustomUnapply(module) then
          stripTyped(selector) match
            case Apply(fun, args)
                if fun.symbol == getSyntheticApply(module) || fun.symbol.denot.isPrimaryConstructor =>
              if args.length == pats.length then
                args.zip(pats).map(matchesRec(_, _, bindings)).reduce(_ and _)
              else
                MatchResult.Disjoint
            case _ =>
              MatchResult.Unknown
        else
          MatchResult.Unknown

      case Typed(unapply: UnApply, _) =>
        matchesRec(selector, unapply, bindings)

      case _ =>
        val selectorSpace = SpaceEngine.project(stripTyped(selector).tpe)
        val patternSpace = SpaceEngine.project(pattern)
        if selectorSpace.isSubspace(patternSpace) then
          MatchResult.Matched(bindings)
        else if SpaceEngine.simplify(SpaceEngine.intersect(selectorSpace, patternSpace)) == EmptySpace then
          MatchResult.Disjoint
        else
          MatchResult.Unknown

  private def stripTyped(tree: Tree): Tree =
    tree match
      case Typed(expr, _) => stripTyped(expr)
      case _              => tree

  private def isCaseClassUnapply(sym: Symbol)(using Context): Boolean =
    sym.name == nme.unapply && sym.owner.is(Flags.Case) && !hasCustomUnapply(sym.owner)

  /** Does the given symbol have a user-written `unapply` method? */
  private def hasCustomUnapply(companion: Symbol)(using Context): Boolean =
    companion.findMember(nme.unapply, NoPrefix, required = Flags.EmptyFlags, excluded = Flags.Synthetic).exists
      || companion.findMember(nme.unapplySeq, NoPrefix, required = Flags.EmptyFlags, excluded = Flags.Synthetic).exists

  private def getSyntheticApply(companion: Symbol)(using Context): Symbol =
    companion.findMember(nme.apply, NoPrefix, required = Flags.Synthetic, excluded = Flags.EmptyFlags).symbol

  private def unfold(tree: Tree)(using Context): Tree =
    args.get(tree.symbol) match
      case Some(tree2) =>
        return transform(tree2)
      case None => ()

    tree.symbol.defTree match
      case valDef: ValDef if !valDef.symbol.is(Flags.Lazy) && isSimple(valDef.rhs) =>
        println(i"Unfolded $tree as ${valDef.rhs}")
        return transform(valDef.rhs)
      case defDef: DefDef if unfoldCalls && isSimple(tree) =>
        val argSyms = defDef.symbol.paramSymss.flatten
        val argTrees = allArguments(tree)
        if argSyms.length == argTrees.length then
          val newArgs = argSyms.zip(argTrees).toMap
          return trace(i"unfold $tree")(recur(defDef.rhs, newArgs))
        ()
      case _ =>
        ()

    tree
