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

/** Removes @qualified annotations. */
def removeAnnotations(using Context) =
  new TypeMap:
    override def apply(tp: Type) =
      tp match
        case AnnotatedType(parent, annot) if annot.symbol == defn.QualifiedAnnot =>
          apply(parent)
        case _ =>
          mapOver(tp)

def addVars(using Context) =
  new TypeMap:
    override def apply(tp: Type) =
      val tp0 =
        tp match
          case tp: AppliedType => derivedAppliedType(tp, tp.tycon, mapArgs(tp.args, tyconTypeParams(tp)))
          case _               => mapOver(tp)
      AnnotatedType(tp0, QualifiedAnnotation(ctx.qualifierSolver.freshVar(), tp0))

def stripQualifiers(using Context) =
  new TypeMap:
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
    val visited = collection.mutable.Set[Singleton]()
    def visit(e: QualifierExpr): Unit =
      e match
        case ref: Ref if !visited(ref) =>
          visited += ref
          val underlyingQualifier = QualifierExprs.fromType(ref.tp.underlying).subst(PredArg, ref)
          assumptions = and(assumptions, underlyingQualifier)
          underlyingQualifier.foreach(visit)
        case _ => ()
    expr.foreach(visit)
    assumptions

  def innerTypes(using Context): List[SingletonType] =
    val buf = new collection.mutable.ListBuffer[SingletonType]
    expr.foreach:
      case Ref(tp) => buf += tp
      case _             => ()
    buf.toList

  def mapTypes(tm: TypeMap)(using Context): QualifierExpr =
    trace[QualifierExpr](res => QualifierLogging.TraceEvent.TypeMap(expr.show, res.show)):
      expr.map:
        case Ref(tp) =>
          tm(tp) match
            case ConstantType(value) => QualifierExprs.fromConst(value)
            case mappedTp: ReferenceType  => Ref(mappedTp)
            case mappedTp                  =>
              // TODO(mbovel): approximate depending on variance
              //throw new Error(f"Expected a singleton named type, but got ${tp} --> ${mappedTp}")
              if tm.variance > 0 then True else False
        case other         => other
