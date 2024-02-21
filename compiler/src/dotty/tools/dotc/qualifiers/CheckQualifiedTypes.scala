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
import dotty.tools.dotc.qualifiers.QualifierLogging.{log, logTreeBefore, logTreeAfter, dumpLogs, enableLogging, disableLogging}
import dotty.tools.dotc.interactive.Interactive.Include.overridden

class CheckQualifiedTypes extends Recheck:
  thisPhase =>

  override def phaseName = "checkQualifiedTypes"

  override def run(using Context): Unit =
    if Feature.qualifiedTypesEnabled then super.run

  override def newRechecker()(using Context): Rechecker = new Rechecker(ctx):
    given QualifierSolver = ctx.qualifierSolver

    /** Removes @qualified annotations in inferred types in the given `unit`. This runs before the recheck* methods
      * below.
      */
    override def checkUnit(unit: CompilationUnit)(using Context) =
      SetupQualifiedTypes(preRecheckPhase, thisPhase, recheckDef).traverse(ctx.compilationUnit.tpdTree)
      super.checkUnit(unit)
      ctx.qualifierSolver.debug()
      instantiateTraverser.traverse(ctx.compilationUnit.tpdTree)

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

    private final var argsQualifier = True
    //private final var argTrees = collection.mutable.ArrayBuffer[Tree]()
    private final var argRefs = collection.mutable.ArrayBuffer[Ref]()
    private final var paramRefs = collection.mutable.ArrayBuffer[Ref]()

    def substQualifierParams(using Context): TypeMap =
      new TypeMap:
        def apply(tp: Type) =
          tp match
            case QualifiedType(parent, qualifier) =>
              val substitutedQualifier =
                qualifier.map { expr =>
                  if paramRefs.contains(expr) then
                    //val replacement = fromTree(argTrees(paramSyms.indexOf(sym)))(using NoSymbol)
                    val replacement = argRefs(paramRefs.indexOf(expr))
                    //log(i"substQualifierParams: $expr --> $replacement")
                    replacement
                  else
                    expr
                }
              tp.derivedQualifiedType(apply(parent), substitutedQualifier)
            case _ =>
              mapOver(tp)

    override protected def recheckArg(arg: tpd.Tree, pt: Type, argSym: Symbol)(using Context): Type =
      //argTrees += arg
      val freshRef = ctx.qualifierSolver.freshRef()
      argRefs += freshRef
      paramRefs += QualifierExprs.fromSymbol(argSym)
      val argTp = super.recheckArg(arg, substQualifierParams(pt), argSym)
      val fact = QualifierExprs.ofType(argTp).subst(PredArg, freshRef)
      ctx.qualifierSolver.assume(fact)
      argsQualifier = QualifierExpr.and(argsQualifier, fact)
      argTp

    override protected def instantiate(mt: MethodType, argTypes: List[Type], sym: Symbol)(using Context): Type =
      val tp = super.instantiate(mt, argTypes, sym)
      // log(i"instantiated: $tp")
      // log(i"facts: $argsQualifier")
      substQualifierParams(tp) match
        case QualifiedType(parent, qualifier) =>
          tp.derivedQualifiedType(parent, QualifierExpr.and(qualifier, argsQualifier))
        case res => res

    override def recheckDefDef(tree: tpd.DefDef, sym: Symbol)(using Context): Type =
      val loggingEnabled = ctx.settings.YdebugQualifiedTypes.value.contains(tree.name.toString)
      if loggingEnabled then
        enableLogging()
        logTreeBefore(Recheck.addRecheckedTypes.transform(tree).show)
      val res = super.recheckDefDef(tree, sym)
      instantiateTraverser.traverse(tree)
      if loggingEnabled then
        logTreeAfter(tree.show)
        dumpLogs(f"qualifier-logging-${tree.name}.json")
        disableLogging()
      res

    override def recheckApply(tree: tpd.Apply, pt: Type)(using Context): Type =
      //log(f"recheckApply(${tree.show}, ${pt.show})")
      val savedArgsQualifier = argsQualifier
      val savedArgRefs = argRefs
      val savedParamSyms = paramRefs
      argsQualifier = True
      argRefs = collection.mutable.ArrayBuffer()
      paramRefs = collection.mutable.ArrayBuffer()
      ctx.qualifierSolver.push()
      val res = super.recheckApply(tree, pt)//.withQualifier(argsQualifier)
      ctx.qualifierSolver.pop()
      argsQualifier = savedArgsQualifier
      argRefs = savedArgRefs
      paramRefs = savedParamSyms
      res

    override def recheckLiteral(tree: tpd.Literal)(using Context): Type =
      val tp = super.recheckLiteral(tree)
      if QualifierExprs.fromConst.isDefinedAt(tree.const) then
        val qualifier = Equal(PredArg, QualifierExprs.fromConst(tree.const))
        tp.withQualifier(qualifier)
      else
        tp

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
