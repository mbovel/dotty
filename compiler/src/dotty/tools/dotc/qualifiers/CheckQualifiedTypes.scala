package dotty.tools
package dotc
package qualifiers

import core.*
import Symbols.*, Contexts.*, Types.*, ContextOps.*, Decorators.*, SymDenotations.*, DenotTransformers.*, Flags.*
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
import QualifierExpr.*
import dotty.tools.dotc.qualifiers.QualifierLogging.{log, logTreeBefore, logTreeSetup, logTreeAfter, dumpLogs, enableLogging, disableLogging}
import dotty.tools.dotc.interactive.Interactive.Include.overridden
import dotty.tools.dotc.cc.Setup

class CheckQualifiedTypes extends Recheck:
  override def phaseName = "checkQualifiedTypes"

  override def run(using Context): Unit =
    if Feature.qualifiedTypesEnabled then super.run

  override def newRechecker()(using Context): Rechecker = new Rechecker(ctx):
    given QualifierSolver = ctx.qualifierSolver

    /** Removes @qualified annotations in inferred types in the given `unit`. This runs before the recheck* methods below.
      */
    override def checkUnit(unit: CompilationUnit)(using Context) =
      val (prevPhase: SetupQualifiedTypes) = (prev: @unchecked)
      SetupQualifiedTypesTraverser(prevPhase, recheckDef).traverse(unit.tpdTree)

       // logging
      enableLogging()
      logTreeBefore(unit.tpdTree.show)
      logTreeSetup(Recheck.addRecheckedTypes.transform(unit.tpdTree).show)

      super.checkUnit(unit)
      instantiateTraverser.traverse(unit.tpdTree)

      // logging
      logTreeAfter(Recheck.addRecheckedTypes.transform(unit.tpdTree).show)
      ctx.qualifierSolver.debug()
      dumpLogs(f"qualifier-logging.json")
      disableLogging()

    val instantiateTraverser = new TreeTraverser:
      override def traverse(tree: Tree)(using Context) =
        traverseChildren(tree)
        tree match
          case tree: TypeTree => tree.rememberTypeAlways(instantiateMap(tree.knownType))
          case _              => ()

    def instantiateMap(using Context) = new TypeMap:
      def apply(t: Type) =
        t match
          case QualifiedType(parent, qualifier) =>
            val instantiated = ctx.qualifierSolver.instantiate(qualifier)
            if instantiated == True then apply(parent)
            else t.derivedQualifiedType(apply(parent), instantiated)
          case _ =>
            mapOver(t)

    def containsQualifiers(tp: Type)(using Context) =
      new TypeAccumulator[Boolean]:
        def apply(acc: Boolean, tp: Type): Boolean =
          acc || (
            tp match
              case QualifiedType(_, _) => true
              case _                   => foldOver(acc, tp)
          )

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

object CheckQualifiedTypes:
  class Pre extends PreRecheck, IdentityDenotTransformer:
    override def isEnabled(using Context) = true
