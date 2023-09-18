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
      qual.println(i"checkQualifiedTypes solver:\n${ctx.qualifierSolver.getClass()}")
      qual.println(i"checkQualifiedTypes setup:\n${Recheck.addRecheckedTypes.transform(ctx.compilationUnit.tpdTree)}")
      super.checkUnit(unit)
      //instantiateTraverser.traverse(ctx.compilationUnit.tpdTree)

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
                    qual.println(i"substQualifierParams: $expr --> $replacement")
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
      // TODO(mbovel): WRONG, should not need to instantiate here
      val fact = ctx.qualifierSolver.instantiate(QualifierExprs.ofType(argTp)).map {
        case PredArg => freshRef
        case pred    => pred
      }
      qual.println(f"fact: $fact")
      argsQualifier = QualifierExpr.and(argsQualifier, fact)
      argTp

    override protected def instantiate(mt: MethodType, argTypes: List[Type], sym: Symbol)(using Context): Type =
      val tp = super.instantiate(mt, argTypes, sym)
      qual.println(i"instantiated: $tp")
      qual.println(i"facts: $argsQualifier")
      substQualifierParams(tp) match
        case QualifiedType(parent, qualifier) =>
          tp.derivedQualifiedType(parent, QualifierExpr.and(qualifier, argsQualifier))
        case res => res



    override def recheckApply(tree: tpd.Apply, pt: Type)(using Context): Type =
      val savedArgRefs = argRefs
      val savedParamSyms = paramRefs
      argRefs = collection.mutable.ArrayBuffer()
      paramRefs = collection.mutable.ArrayBuffer()
      val res = super.recheckApply(tree, pt)
      argRefs = savedArgRefs
      paramRefs = savedParamSyms
      res

    override def checkConformsExpr(actual: Type, expected: Type, tree: tpd.Tree)(using Context): Unit =
      qual.println(i"facts: $argsQualifier")
      qual.println(i"checkConformsExpr: $actual <:< $expected")
      super.checkConformsExpr(actual, expected, tree)

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
