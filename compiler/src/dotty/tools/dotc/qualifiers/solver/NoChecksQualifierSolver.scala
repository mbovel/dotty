package dotty.tools
package dotc
package qualifiers
package solver

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*
import scala.collection.mutable
import QualifierExpr.*
import dotty.tools.dotc.util.SimpleIdentitySet.empty
import dotty.tools.dotc.core.Constants.Constant
import config.Printers.qual

class NoChecksQualifierSolver extends QualifierSolver:
  override final def tryImply(from: QualifierExpr, to: QualifierExpr): Boolean =
    val res = recur(from, to, frozen = true) || recur(from, to, frozen = false)
    qual.println(s"tryImply($from, $to) == $res\n---")
    res

  private final def recur(from: QualifierExpr, to: QualifierExpr, frozen: Boolean): Boolean =
    val res = maybeRollback {
      to match
        case to: Var =>
          hasImplicationToVar(from, to) || (!frozen && tryAddImplicationToVar(from, to))
        case to: And =>
          to.args.forall(recur(from, _, frozen))
        case to: Or =>
          to.args.exists(recur(from, _, frozen))
        case _ =>
          assert(!to.hasVars)
          from match
            case from: Var =>
              hasImplicationToLeaf(from, to) || (!frozen && tryAddImplicationToLeaf(from, to))
            case from: And =>
              from.args.exists(recur(_, to, frozen))
            case from: Or  =>
              from.args.forall(recur(_, to, frozen))
            case _ => leafImplies(from, to, frozen)
    }
    qual.println(s"recur($from, $to, frozen = $frozen) == $res")
    res

  protected def leafImplies(from: QualifierExpr, to: QualifierExpr, frozen: Boolean): Boolean =
    if frozen then from.equiv(to) else !from.equiv(True)


  /**************/
  /* Vars state */
  /**************/

  private case class ImplicationToVar(premise: QualifierExpr, conclusion: Var)
  private case class ImplicationToLeaf(premise: Var, conclusion: QualifierExpr /* with !it.hasVars */ )

  /** The value associated to a given key is an array of all ImplicationToVar that have this key as their conclusion. */
  private val implicationsToVars = mutable.Map.empty[Var, Set[ImplicationToVar]]

  /** The value associated to a given key is an array of all ImplicationToVar that have this key in their premise. */
  private val dependencies = mutable.Map.empty[Var, Set[ImplicationToVar]]

  /** The value associated to a given key is an array of all ImplicationToLeaf that have this key in their premise. */
  private val implicationsToLeafs = mutable.Map.empty[Var, Set[ImplicationToLeaf]]

  /** Journal of all implicationsToVar that have been added. Used to rollback if needed. */
  private val implicationsToVarJournal = mutable.ArrayBuffer[ImplicationToVar]()

  /** Journal of all implicationsToVar that have been added. Used to rollback if needed. */
  private val implicationsToLeafJournal = mutable.ArrayBuffer[ImplicationToLeaf]()


  override final def instantiate(p: QualifierExpr): QualifierExpr =
    // TODO(mbovel): memoize (and invalidate when adding premises).
    p.map {
      case v: Var =>
        implicationsToVars
          .getOrElse(v, Set.empty)
          .map(impl => instantiate(impl.premise))
          .fold(False)(or)
      case p =>
        p
    }

  final def hasImplicationToVar(premise: QualifierExpr, conclusion: Var): Boolean =
    implicationsToVars.getOrElse(conclusion, Set.empty).contains(ImplicationToVar(premise, conclusion))

  final def tryAddImplicationToVar(premise: QualifierExpr, conclusion: Var): Boolean =
    val implication = ImplicationToVar(premise, conclusion)
    val previousImplications = implicationsToVars.getOrElse(conclusion, Set.empty)
    implicationsToVars.update(conclusion, previousImplications+ implication)

    val canAdd =
      implicationsToLeafs.getOrElse(conclusion, Set.empty)
                         .forall(impl => recur(premise, impl.conclusion, frozen = false))
      && dependencies.getOrElse(conclusion, Set.empty)
                     .forall(impl => recur(impl.premise.map(instantiate), premise, frozen = false))

    if canAdd then
      for premiseVar <- premise.vars do
        dependencies.update(premiseVar, dependencies.getOrElse(premiseVar, Set.empty) + implication)
      implicationsToVarJournal.append(implication)
    else
      implicationsToVars.update(conclusion, previousImplications)

    qual.println(s"tryAddImplicationToVar($premise, $conclusion) == $canAdd")
    canAdd

  private final def removeImplicationToVar(premise: QualifierExpr, conclusion: Var): Unit =
    val implication = ImplicationToVar(premise, conclusion)
    implicationsToVars.update(conclusion, implicationsToVars(conclusion) - implication)
    for premiseVar <- premise.vars do
      dependencies.update(premiseVar, dependencies(premiseVar) - implication)

  final def hasImplicationToLeaf(premise: Var, conclusion: QualifierExpr /* with !it.hasVars */ ): Boolean =
    implicationsToLeafs.getOrElse(premise, Set.empty).contains(ImplicationToLeaf(premise, conclusion))

  final def tryAddImplicationToLeaf(premise: Var, conclusion: QualifierExpr /* with !it.hasVars */ ): Boolean =
    val implication = ImplicationToLeaf(premise, conclusion)
    val canAdd = recur(premise.map(instantiate), conclusion, frozen = false)
    if canAdd then
      val previousImplications = implicationsToLeafs.getOrElse(premise, Set.empty)
      implicationsToLeafs.update(premise, previousImplications + implication)
      implicationsToLeafJournal.append(implication)
    qual.println(s"tryAddImplicationToLeaf($premise, $conclusion) == $canAdd")
    canAdd

  private final def removeImplicationToLeaf(premise: Var, conclusion: QualifierExpr /* with !it.hasVars */ ): Unit =
    val implication = ImplicationToLeaf(premise, conclusion)
    implicationsToLeafs.update(premise, implicationsToLeafs(premise) - implication)

  inline def maybeRollback(inline f: => Boolean): Boolean =
    val prevImplicationsToVarJournalSize = implicationsToVarJournal.size
    val prevImplicationsToLeafJournalSize = implicationsToLeafJournal.size
    val res = f
    if !res then
      for i <- prevImplicationsToVarJournalSize until implicationsToVarJournal.size do
        val ImplicationToVar(premise, conclusion) = implicationsToVarJournal(i)
        removeImplicationToVar(premise, conclusion)
      implicationsToVarJournal.takeInPlace(prevImplicationsToVarJournalSize)
      for i <- prevImplicationsToLeafJournalSize until implicationsToLeafJournal.size do
        val ImplicationToLeaf(premise, conclusion) = implicationsToLeafJournal(i)
        removeImplicationToLeaf(premise, conclusion)
      implicationsToLeafJournal.takeInPlace(prevImplicationsToLeafJournalSize)
    res


