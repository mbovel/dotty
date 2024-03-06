package dotty.tools.dotc.qualifiers
package solver

import collection.mutable
import QualifierExpr.*
import QualifierLogging.log
import math.Ordering.Implicits.{seqOrdering, infixOrderingOps}
import dotty.tools.dotc.qualifiers.QualifierLogging.{trace, startTrace, endTrace, LogEvent, LogEventNode, logNaiveSolverVars}

class NaiveQualifierSolver extends QualifierSolver:
  final var contextStack = List(True)

  override final def push(): Unit =
    startTrace(LogEvent.Push(contextStack.mkString(", ")))
    contextStack = contextStack.head :: contextStack

  override final def pop(): Unit =
    endTrace()
    contextStack = contextStack.tail

  override final def assume(p: QualifierExpr): Unit =
    val head = and(contextStack.head, p)
    trace(LogEvent.Assume(p))
    contextStack = head :: contextStack.tail

  override final def check(to: QualifierExpr): Boolean =
    val from = contextStack.head
    trace(res => LogEvent.Check(from, to, res)):
      tryImply(from, to, frozen = true) || tryImply(from, to, frozen = false)

  private final def tryImply(from: QualifierExpr, to: QualifierExpr, frozen: Boolean): Boolean =
    trace(res => LogEvent.TryImply(from, to, frozen, res)):
      maybeRollback:
        to match
          case to: ApplyVar =>
            hasImplicationToVar(from, to) || (!frozen && tryAddImplicationToVar(from, to))
          case to: And =>
            to.args.forall(tryImply(from, _, frozen))
          case _ =>
            from match
              case from: Or =>
                from.args.forall(tryImply(_, to, frozen))
              case _ =>
                to match
                  case to: Or =>
                    to.args.exists(tryImply(from, _, frozen))
                  case _ =>
                    assert(!to.hasVars)
                    from match
                      case from if from.hasVars =>
                        hasImplicationToLeaf(from, to) || (!frozen && tryAddImplicationToLeaf(from, to))
                      case _ =>
                        assert(!from.hasVars)
                        leafImplies(from, to, frozen)

  protected def leafImplies(
      rootFrom: QualifierExpr /* with !it.hasVars */,
      rootTo: QualifierExpr /* with !it.hasVars */,
      frozen: Boolean
  ): Boolean =
    def rec(from: QualifierExpr, to: QualifierExpr): Boolean =
      to == True || from == False || (
        from match
          case from: And =>
            from.args.exists(rec(_, to))
          case _ =>
            from == to
      )

    trace(res => LogEvent.LeafImplies(rootFrom, rootTo, res)):
      rec(rootFrom, rootTo) || {
        val eqs = NaiveQualifierEquivalenceEngine(rootFrom)
        val rewrittenTo = eqs.rewrite(rootTo)
        trace(res => LogEvent.LeafImpliesEquiv(eqs.premise, rewrittenTo, res)):
          rec(eqs.premise, rewrittenTo)
      }

  /*------------*/
  /* Vars state */
  /*------------*/

  private case class ImplicationToVar(premise: QualifierExpr, conclusion: ApplyVar):
    override def toString(): String = f"$premise ->  $conclusion"
  private case class ImplicationToLeaf(premise: QualifierExpr, conclusion: QualifierExpr /* with !conclusion.hasVars */ ):
    override def toString(): String = f"$premise ->  $conclusion"

  /** The value associated to a given key is the set of ImplicationToVar that have this key as their conclusion.
    */
  private val implicationsToVars = mutable.Map.empty[Int, Set[ImplicationToVar]]

  /** The value associated to a given key is the set of ImplicationToVar that have this key in their premise.
    */
  private val dependencies = mutable.Map.empty[Int, Set[ImplicationToVar]]

  /** The value associated to a given key is the set of ImplicationToLeaf that have this key in their premise.
    */
  private val implicationsToLeafs = mutable.Map.empty[Int, Set[ImplicationToLeaf]]

  /** Journal of all implicationsToVar that have been added. Used to rollback if needed. */
  private val implicationsToVarJournal = mutable.ArrayBuffer[ImplicationToVar]()

  /** Journal of all implicationsToLeaf that have been added. Used to rollback if needed. */
  private val implicationsToLeafJournal = mutable.ArrayBuffer[ImplicationToLeaf]()

  override final def instantiate(p: QualifierExpr): QualifierExpr =
    // TODO(mbovel): memoize (and invalidate when adding premises).
    p.map {
      case v: ApplyVar =>
        implicationsToVars
          .getOrElse(v.i, Set.empty)
          .map(impl => instantiate(impl.premise.subst(impl.conclusion.arg, v.arg)))
          .fold(False)(or)
      case p =>
        p
    }

  final def hasImplicationToVar(premise: QualifierExpr, conclusion: ApplyVar): Boolean =
    implicationsToVars.getOrElse(conclusion.i, Set.empty).contains(ImplicationToVar(
      premise,
      conclusion
    ))

  final def tryAddImplicationToVar(premise: QualifierExpr, conclusion: ApplyVar): Boolean =
    trace(res => LogEvent.TryAddImplicationToVar(premise, conclusion, res)):
      val implication = ImplicationToVar(premise, conclusion)
      val previousImplications = implicationsToVars.getOrElse(conclusion.i, Set.empty)
      implicationsToVars.update(conclusion.i, previousImplications + implication)

      val canAdd =
        implicationsToLeafs.getOrElse(conclusion.i, Set.empty)
          .forall(impl => tryImply(instantiate(impl.premise), impl.conclusion, frozen = false))
          && dependencies.getOrElse(conclusion.i, Set.empty)
            .forall(impl => tryImply(instantiate(impl.premise), impl.conclusion, frozen = false))

      if canAdd then
        for premiseVar <- premise.vars do
          dependencies.update(premiseVar.i, dependencies.getOrElse(premiseVar.i, Set.empty) + implication)
        implicationsToVarJournal.append(implication)
      else
        implicationsToVars.update(conclusion.i, previousImplications)

      canAdd

  private final def removeImplicationToVar(premise: QualifierExpr, conclusion: ApplyVar): Unit =
    val implication = ImplicationToVar(premise, conclusion)
    implicationsToVars.update(conclusion.i, implicationsToVars(conclusion.i) - implication)
    for premiseVar <- premise.vars do
      dependencies.update(premiseVar.i, dependencies(premiseVar.i) - implication)

  final def hasImplicationToLeaf(
      premise: QualifierExpr /* with it.hasVars */,
      conclusion: QualifierExpr /* with !it.hasVars */
  ): Boolean =
    // TODO(mbovel): wrong; should split ands correctly.
    val firstVar = premise.vars.head
    implicationsToLeafs
      .getOrElse(firstVar.i, Set.empty)
      .contains(ImplicationToLeaf(premise, conclusion))

  final def tryAddImplicationToLeaf(
      premise: QualifierExpr /* with it.hasVars */,
      conclusion: QualifierExpr /* with !it.hasVars */
  ): Boolean =
    trace(res => LogEvent.TryAddImplicationToLeaf(premise, conclusion, res)):
      val implication = ImplicationToLeaf(premise, conclusion)
      val canAdd = tryImply(instantiate(premise), conclusion, frozen = false)
      if canAdd then
        for premiseVar <- premise.vars do
          val previousImplications = implicationsToLeafs.getOrElse(premiseVar.i, Set.empty)
          implicationsToLeafs.update(premiseVar.i, previousImplications + implication)
        implicationsToLeafJournal.append(implication)
      canAdd

  private final def removeImplicationToLeaf(
      premise: QualifierExpr,
      conclusion: QualifierExpr /* with !it.hasVars */
  ): Unit =
    val implication = ImplicationToLeaf(premise, conclusion)
    for premiseVar <- premise.vars do
      implicationsToLeafs.update(premiseVar.i, implicationsToLeafs(premiseVar.i) - implication)

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

  override def debug(): Unit =
    val dependencies =
      implicationsToVars.values.flatten.map(imp => IArray(imp.premise, imp.conclusion))
      ++ implicationsToLeafs.values.flatten.map(imp => IArray(imp.premise, imp.conclusion))
    logNaiveSolverVars(IArray.from(dependencies))
