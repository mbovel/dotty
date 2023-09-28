package dotty.tools
package dotc
package qualifiers

import core.*
import Symbols.*, Contexts.*, Types.*, ContextOps.*, Decorators.*,
SymDenotations.*, DenotTransformers.*, Flags.*
import ast.tpd.*
import Names.Name
import Phases.Phase
import transform.{Recheck, PreRecheck}
import config.{Config, Feature}
import config.Printers.qual
import Recheck.*
import annotation.constructorOnly
import ast.tpd
import solver.QualifierSolver

class CheckQualifiedTypes extends Recheck:
  thisPhase =>

  import tpd.*

  override def phaseName = "checkQualifiedTypes"

  override def run(using Context): Unit =
    if Feature.qualifiedTypesEnabled then super.run

  override def newRechecker()(using Context): Rechecker = new Rechecker(ctx):
    given QualifierSolver = ctx.qualifierSolver

    /** Removes @qualified annotations in inferred types in the given `unit`. This
      * runs before the recheck* methods below.
      */
    override def checkUnit(unit: CompilationUnit)(using Context) =
      setupTraverser.traverse(ctx.compilationUnit.tpdTree)
      qual.println(i"checkQualifiedTypes solver:\n${ctx.qualifierSolver.getClass()}")
      qual.println(i"checkQualifiedTypes setup:\n${Recheck.addRecheckedTypes.transform(ctx.compilationUnit.tpdTree)}")
      super.checkUnit(unit)
      instantiateTraverser.traverse(ctx.compilationUnit.tpdTree)

    /** Removes @qualified annotations in inferred types.
      */
    val setupTraverser = new TreeTraverser:
      override def traverse(tree: Tree)(using Context) =
        traverseChildren(tree)
        tree match
          case tree: Block =>
            tree match
              case closureDef(mdef) =>
                val msym = mdef.symbol
                val mt = msym.info.asInstanceOf[MethodType]
                val newResType =
                  if !mdef.tpt.isInstanceOf[InferredTypeTree] then
                    addVar(mdef.tpt.knownType)
                  else
                    mdef.tpt.knownType
                val newInfo = mt.companion(mt.paramInfos, newResType)
                val completer = new LazyType:
                  def complete(denot: SymDenotation)(using Context) =
                    denot.info = newInfo
                    recheckDef(mdef, msym)

                msym.updateInfoBetween(preRecheckPhase, thisPhase, completer)
                mdef.tpt.rememberTypeAlways(newResType)

                val closure(env, meth, tpt) = tree: @unchecked
                tpt.knownType match
                  case defn.FunctionOf(params, restpe, isContextual) =>
                    tpt.rememberTypeAlways(defn.FunctionOf(params, newResType, isContextual))
                  case _ => ()
              case _ => ()
          case tree: InferredTypeTree =>
            tree.rememberType(addVar(removeRefineAnnotTypeMap(tree.knownType)))
          case tree: TypeTree =>
            tree.rememberType(normalizeAnnotations(tree.knownType))
          case _ => ()

    def addVar(tp: Type)(using Context): Type =
      AnnotatedType(tp, QualifiedAnnotation(ctx.qualifierSolver.freshVar()))

    /** Removes @qualified annotations.
      */
    val removeRefineAnnotTypeMap = new TypeMap:
      def apply(t: Type) =
        t match
          case AnnotatedType(parent, annot)
              if annot.symbol == defn.QualifiedAnnot =>
            apply(parent)
          case _ =>
            mapOver(t)

    /** Instantiate all inferred qualifiers.
      */
    val instantiateTraverser = new TreeTraverser:
      override def traverse(tree: Tree)(using Context) =
        traverseChildren(tree)
        tree match
          case tree: TypeTree =>
            tree.rememberTypeAlways(instantiateMap(tree.knownType))
          case _ => ()

    val normalizeAnnotations = new TypeMap:
      def apply(t: Type) =
        t match
          case QualifiedType(parent, qualifier) =>
            AnnotatedType(apply(parent), QualifiedAnnotation(qualifier))
          case _ =>
            mapOver(t)

    /** Removes @refined annotations.
      */
    def instantiateMap(using Context) = new TypeMap:
      def apply(t: Type) =
        t match
          case QualifiedType(parent, qualifier) =>
            val instantiated = ctx.qualifierSolver.instantiate(qualifier)
            if instantiated == QualifierExpr.True then apply(parent)
            else t.derivedQualifiedType(apply(parent), instantiated)
          case _ =>
            mapOver(t)

    override def recheckTypeTree(tree: tpd.TypeTree)(using Context): Type =
      super.recheckTypeTree(tree) match
        case tp: AnnotatedType
            if ctx.phase == Phases.checkQualifiersPhase
              && tp.annot.symbol == defn.QualifiedAnnot =>
          tp.annot match
            case _: QualifiedAnnotation => tree.knownType
            case annot =>
              annot.tree.knownType match
                case AppliedType(_, List(arg)) =>
                  if arg != tp.parent then
                    val msg =
                      em"Malformed refinement. Expected a refinement of ${tp.parent.show} but got ${arg.show}."
                    report.error(msg, tp.annot.tree.sourcePos)
                    ErrorType(msg)
                  else tree.knownType
        case _ => tree.knownType

  /** Update info of `sym` for CheckRefinements phase only */
  private def updateInfo(sym: Symbol, info: Type)(using Context) =
    sym.updateInfoBetween(preRecheckPhase, thisPhase, info)

    /*
    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Unit =
      tree.termParamss
        .flatten
        .flatMap(param => RefinementType.unapply(param.tpe))
        .foreach(checkPredicateFormat)

      RefinementType.unapply(tree.tpe)
        .foreach(checkPredicateFormat)
     */

object CheckQualifiedTypes:
  class Pre extends PreRecheck, IdentityDenotTransformer:
    override def isEnabled(using Context) = true
