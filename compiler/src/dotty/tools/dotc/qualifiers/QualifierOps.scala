package dotty.tools
package dotc
package qualifiers

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*

import QualifierExpr.*
import QualifierLogging.trace

extension (tp: Type)
  def qualifierImplies(that: Type)(using Context): Boolean =
    val thisQualifier = QualifierExprs.fromType(tp)
    val thatQualifier = QualifierExprs.fromType(that)
    ctx.qualifierSolver.tryImply(
      and(thisQualifier, and(thisQualifier.getRefsContext, thatQualifier.getRefsContext)),
      thatQualifier
    )

  def withQualifier(newQualifier: QualifierExpr)(using Context) =
    tp match
      case QualifiedType(parent, qualifier) =>
        tp.derivedQualifiedType(parent, QualifierExpr.and(qualifier, newQualifier))
      case res =>
        QualifiedType(res, newQualifier)

  /** @pre `tp` is a QualifiedType */
  def derivedQualifiedType(parent: Type, refinement: QualifierExpr)(using Context): Type =
    tp match
      case tp @ QualifiedType(p, r) =>
        if (parent eq p) && (refinement eq r) then tp
        else QualifiedType(parent, refinement)

  def stripRefinements(using Context): Type =
    stripRefinementsMap(tp)

private def stripRefinementsMap(using Context) = new TypeMap:
  override def apply(tp: Type): Type =
    tp.dealias match
      case tp: AppliedType =>
        derivedAppliedType(tp, this(tp.tycon), tp.args)
      case tp: RefinedType =>
        derivedRefinedType(tp, this(tp.parent), tp.refinedInfo)
      case OrType(tp1, tp2) =>
        if tp1 frozen_=:= tp2 then mapOver(tp)
        else tp
      case QualifiedType(parent, refinement) =>
        this(parent)
      case _ =>
        mapOver(tp)

extension (expr: QualifierExpr)
  def getRefsContext(using Context): QualifierExpr =
    var assumptions = True
    val visitedRefs = collection.mutable.Set[Ref]()
    def visit(e: QualifierExpr): Unit =
      e match
        case ref: Ref if !visitedRefs(ref) =>
          visitedRefs += ref
          val pred = QualifierExprs.fromType(ref.tp).subst(PredArg, ref)
          assumptions = and(assumptions, pred)
          pred.foreach(visit)
        case _ => ()
    expr.foreach(visit)
    assumptions

  def innerTypes(using Context): List[SingletonType] =
    val buf = new collection.mutable.ListBuffer[SingletonType]
    expr.foreach:
      case ref: Ref => buf += ref.tp
      case _        => ()
    buf.toList

  def typeMap(tm: TypeMap)(using Context): QualifierExpr =
    trace[QualifierExpr](res => QualifierLogging.LogEvent.TypeMap(expr.show, res.show)):
      expr.map:
        case ref: Ref =>
          val newTp: SingletonType =
            tm(ref.tp) match
              case tp: SingletonType => tp
              case tp                => SkolemType(tp)
          Ref(newTp)
        case other => other
