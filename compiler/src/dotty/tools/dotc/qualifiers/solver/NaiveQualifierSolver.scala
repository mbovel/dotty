package dotty.tools.dotc.qualifiers
package solver

import collection.mutable
import QualifierExpr.*
import math.Ordering.Implicits.{seqOrdering, infixOrderingOps}
import utils.EqClasses

class NaiveQualifierSolver extends QualifierSolver:
  import NaiveQualifierSolver.*

  override final def tryImply(from: QualifierExpr, to: QualifierExpr): Boolean =
    val res = _tryImply(from, to, frozen = true) || _tryImply(from, to, frozen = false)
    log(s"tryImply($from, $to) == $res\n---")
    res

  private final def _tryImply(from: QualifierExpr, to: QualifierExpr, frozen: Boolean): Boolean =
    val res = maybeRollback {
      to match
        case to: Var =>
          hasImplicationToVar(from, to) || (!frozen && tryAddImplicationToVar(from, to))
        case to: And =>
          to.args.forall(_tryImply(from, _, frozen))
        case _ =>
          from match
            case from: Or =>
              from.args.forall(_tryImply(_, to, frozen))
            case from: And if from.hasVars =>
              from.args.exists(_tryImply(_, to, frozen))
            case _ =>
              to match
                case to: Or =>
                  to.args.exists(_tryImply(from, _, frozen))
                case _ =>
                  assert(!to.hasVars)
                  from match
                    case from: Var =>
                      hasImplicationToLeaf(from, to) || (!frozen && tryAddImplicationToLeaf(
                        from,
                        to
                      ))
                    case _ =>
                      assert(!from.hasVars)
                      leafImplies(from, to, frozen)
    }
    log(s"_tryImply($from, $to, frozen = $frozen) == $res")
    res

  protected def leafImplies(
      rootFrom: QualifierExpr /* with !it.hasVars */,
      rootTo: QualifierExpr /* with !it.hasVars */,
      frozen: Boolean
  ): Boolean =
    log(s"leafImplies($rootFrom, $rootTo, frozen = $frozen)")

    def recur(from: QualifierExpr /* with !it.hasVars */, to: QualifierExpr /* with !it.hasVars */): Boolean =
      to.equiv(True) || (
        from match
          case from: And =>
            from.args.exists(recur(_, to))
          case _ =>
            from.equiv(rootTo) || from.equiv(False)
      )

    def tryRewrite(): Boolean =
      log(s"tryRewrite")
      val (rewrittenFrom, eqs) = getEqClasses(rootFrom)
      recur(rewrittenFrom, rewrite(rootTo, eqs))

    recur(rootFrom, rootTo) || tryRewrite()

  /*------------*/
  /* Vars state */
  /*------------*/

  private case class ImplicationToVar(premise: QualifierExpr, conclusion: Var)
  private case class ImplicationToLeaf(premise: Var, conclusion: QualifierExpr /* with !it.hasVars */ )

  /** The value associated to a given key is an array of all ImplicationToVar that have this key as
    * their conclusion.
    */
  private val implicationsToVars = mutable.Map.empty[Var, Set[ImplicationToVar]]

  /** The value associated to a given key is an array of all ImplicationToVar that have this key in
    * their premise.
    */
  private val dependencies = mutable.Map.empty[Var, Set[ImplicationToVar]]

  /** The value associated to a given key is an array of all ImplicationToLeaf that have this key in
    * their premise.
    */
  private val implicationsToLeafs = mutable.Map.empty[Var, Set[ImplicationToLeaf]]

  /** Journal of all implicationsToVar that have been added. Used to rollback if needed. */
  private val implicationsToVarJournal = mutable.ArrayBuffer[ImplicationToVar]()

  /** Journal of all implicationsToLeaf that have been added. Used to rollback if needed. */
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
    implicationsToVars.getOrElse(conclusion, Set.empty).contains(ImplicationToVar(
      premise,
      conclusion
    ))

  final def tryAddImplicationToVar(premise: QualifierExpr, conclusion: Var): Boolean =
    val implication = ImplicationToVar(premise, conclusion)
    val previousImplications = implicationsToVars.getOrElse(conclusion, Set.empty)
    implicationsToVars.update(conclusion, previousImplications + implication)

    val canAdd =
      implicationsToLeafs.getOrElse(conclusion, Set.empty)
        .forall(impl => _tryImply(premise, impl.conclusion, frozen = false))
        && dependencies.getOrElse(conclusion, Set.empty)
          .forall(impl => _tryImply(impl.premise.map(instantiate), premise, frozen = false))

    if canAdd then
      for premiseVar <- premise.vars do
        dependencies.update(premiseVar, dependencies.getOrElse(premiseVar, Set.empty) + implication)
      implicationsToVarJournal.append(implication)
    else
      implicationsToVars.update(conclusion, previousImplications)

    log(s"tryAddImplicationToVar($premise, $conclusion) == $canAdd")
    canAdd

  private final def removeImplicationToVar(premise: QualifierExpr, conclusion: Var): Unit =
    val implication = ImplicationToVar(premise, conclusion)
    implicationsToVars.update(conclusion, implicationsToVars(conclusion) - implication)
    for premiseVar <- premise.vars do
      dependencies.update(premiseVar, dependencies(premiseVar) - implication)

  final def hasImplicationToLeaf(premise: Var, conclusion: QualifierExpr /* with !it.hasVars */ ): Boolean =
    implicationsToLeafs.getOrElse(premise, Set.empty).contains(ImplicationToLeaf(
      premise,
      conclusion
    ))

  final def tryAddImplicationToLeaf(
      premise: Var,
      conclusion: QualifierExpr /* with !it.hasVars */
  ): Boolean =
    val implication = ImplicationToLeaf(premise, conclusion)
    val canAdd = _tryImply(premise.map(instantiate), conclusion, frozen = false)
    if canAdd then
      val previousImplications = implicationsToLeafs.getOrElse(premise, Set.empty)
      implicationsToLeafs.update(premise, previousImplications + implication)
      implicationsToLeafJournal.append(implication)
    log(s"tryAddImplicationToLeaf($premise, $conclusion) == $canAdd")
    canAdd

  private final def removeImplicationToLeaf(
      premise: Var,
      conclusion: QualifierExpr /* with !it.hasVars */
  ): Unit =
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

object NaiveQualifierSolver:
  /*----------*/
  /* Equality */
  /*----------*/

  private[solver] final def rewrite(expr: QualifierExpr, eqs: EqClasses[QualifierExpr]): QualifierExpr =
    val result = expr.map(eqs.repr)
    log(s"rewrite($expr, $eqs) == $result")
    if result != expr then rewrite(result, eqs) else result

  private[solver] final def getEqClasses(startFrom: QualifierExpr): (And, EqClasses[QualifierExpr]) =
    val eqClasses = EqClasses[QualifierExpr]()
    var from: And = topAnd(startFrom)
    var changed = true
    while changed do
      changed = false
      from.foreach {
        case Equal(from, to) => changed |= eqClasses.addEq(from, to)
        case _ => ()
      }
      from = topAnd(rewrite(from, eqClasses))
      log(s"getEqClasses step: $eqClasses")
    (from, eqClasses)

  /*-------*/
  /* Utils */
  /*-------*/

  private inline def log(inline msg: => String): Unit =
    println(msg)
    ()

