package dotty.tools.dotc.qualifiers
package solver

import collection.mutable
import QualifierExpr.*
import QualifierLogging.log
import math.Ordering.Implicits.{seqOrdering, infixOrderingOps}

class NaiveQualifierSolver extends QualifierSolver:
  final var contextStack = List(True)

  override final def push(): Unit =
    log("push()")
    contextStack = contextStack.head :: contextStack

  override final def pop(): Unit =
    log(f"pop(${contextStack.head}) --> ${contextStack.tail}")
    contextStack = contextStack.tail

  override final def assume(p: QualifierExpr): Unit =
    val head = and(contextStack.head, p)
    log(f"assume($p) --> $head")
    contextStack = head :: contextStack.tail

  override final def check(to: QualifierExpr): Boolean =
    val from = contextStack.head
    val res = tryImplyRec(from, to, frozen = true) || tryImplyRec(from, to, frozen = false)
    log(s"tryImply($from, $to) == $res\n---")
    res

  private final def tryImplyRec(from: QualifierExpr, to: QualifierExpr, frozen: Boolean): Boolean =
    val res = maybeRollback {
      to match
        case to: ApplyVar =>
          hasImplicationToVar(from, to) || (!frozen && tryAddImplicationToVar(from, to))
        case to: And =>
          to.args.forall(tryImplyRec(from, _, frozen))
        case _ =>
          from match
            case from: Or =>
              from.args.forall(tryImplyRec(_, to, frozen))
            case _ =>
              to match
                case to: Or =>
                  to.args.exists(tryImplyRec(from, _, frozen))
                case _ =>
                  assert(!to.hasVars)
                  from match
                    case from if from.hasVars =>
                      hasImplicationToLeaf(from, to)
                      || (!frozen && tryAddImplicationToLeaf(from, to))
                    case _ =>
                      assert(!from.hasVars)
                      leafImplies(from, to, frozen)
    }
    log(s"tryImplyRec($from, $to, frozen = $frozen) == $res")
    res

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

    val res = rec(rootFrom, rootTo) || {
      val eqs = NaiveQualifierEquivalenceEngine(rootFrom)
      rec(eqs.premise, eqs.rewrite(rootTo))
    }

    log(s"leafImplies($rootFrom, $rootTo, frozen = $frozen) == $res")
    res

  /*------------*/
  /* Vars state */
  /*------------*/

  private case class ImplicationToVar(premise: QualifierExpr, conclusion: ApplyVar)
  private case class ImplicationToLeaf(premise: QualifierExpr, conclusion: QualifierExpr /* with !it.hasVars */ )

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
    val implication = ImplicationToVar(premise, conclusion)
    val previousImplications = implicationsToVars.getOrElse(conclusion.i, Set.empty)
    implicationsToVars.update(conclusion.i, previousImplications + implication)

    val canAdd =
      implicationsToLeafs.getOrElse(conclusion.i, Set.empty)
        .forall(impl => tryImplyRec(instantiate(impl.premise), impl.conclusion, frozen = false))
        && dependencies.getOrElse(conclusion.i, Set.empty)
          .forall(impl => tryImplyRec(instantiate(impl.premise), impl.conclusion, frozen = false))

    if canAdd then
      for premiseVar <- premise.vars do
        dependencies.update(premiseVar.i, dependencies.getOrElse(premiseVar.i, Set.empty) + implication)
      implicationsToVarJournal.append(implication)
    else
      implicationsToVars.update(conclusion.i, previousImplications)

    log(s"tryAddImplicationToVar($premise, $conclusion) == $canAdd")
    canAdd

  private final def removeImplicationToVar(premise: QualifierExpr, conclusion: ApplyVar): Unit =
    val implication = ImplicationToVar(premise, conclusion)
    implicationsToVars.update(conclusion.i, implicationsToVars(conclusion.i) - implication)
    for premiseVar <- premise.vars do
      dependencies.update(premiseVar.i, dependencies(premiseVar.i) - implication)

  final def hasImplicationToLeaf(premise: QualifierExpr /* with it.hasVars */, conclusion: QualifierExpr /* with !it.hasVars */ ): Boolean =
    val firstVar = premise.vars.head
    implicationsToLeafs
      .getOrElse(firstVar.i, Set.empty)
      .contains(ImplicationToLeaf(premise, conclusion))

  final def tryAddImplicationToLeaf(
      premise: QualifierExpr /* with it.hasVars */,
      conclusion: QualifierExpr /* with !it.hasVars */
  ): Boolean =
    val implication = ImplicationToLeaf(premise, conclusion)
    val canAdd = tryImplyRec(instantiate(premise), conclusion, frozen = false)
    if canAdd then
      for premiseVar <- premise.vars do
        val previousImplications = implicationsToLeafs.getOrElse(premiseVar.i, Set.empty)
        implicationsToLeafs.update(premiseVar.i, previousImplications + implication)
      implicationsToLeafJournal.append(implication)
    log(s"tryAddImplicationToLeaf($premise, $conclusion) == $canAdd")
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
    println("Vars state:")
    println(s"implicationsToVars" + implicationsToVars.mkString("\n  ", ",\n  ", "\n"))
    println(s"implicationsToLeafs" + implicationsToLeafs.mkString("\n  ", ",\n  ", "\n"))
    println()
