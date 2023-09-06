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

final class SetupQualifiedTypes(
  preRecheckPhase: DenotTransformer,
  thisPhase: DenotTransformer,
  recheckDef: (tpd.ValOrDefDef, Symbol) => Context ?=> Unit
) extends TreeTraverser:
  /** Removes @qualified annotations in inferred types.
    */
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
                addVars(mdef.tpt.knownType)
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
        tree.rememberType(addVars(removeRefineAnnotTypeMap(tree.knownType)))
      case tree: TypeTree =>
        tree.rememberType(normalizeAnnotations(tree.knownType))
      case _ => ()

    updateTreeInfo(tree)

  def normalizeAnnotations(using Context) = new TypeMap:
    override def apply(tp: Type) =
      tp match
        case QualifiedType(parent, qualifier) =>
          AnnotatedType(apply(parent), QualifiedAnnotation(qualifier))
        case _ =>
          mapOver(tp)

  /** Removes @qualified annotations.
    */
  def removeRefineAnnotTypeMap(using Context) = new TypeMap:
    override def apply(tp: Type) =
      tp match
        case AnnotatedType(parent, annot) if annot.symbol == defn.QualifiedAnnot =>
          apply(parent)
        case _ =>
          mapOver(tp)

  def addVars(using Context) = new TypeMap:
    override def apply(tp: Type) =
      val tp0 = tp match
        case tp: AppliedType =>
          derivedAppliedType(tp, tp.tycon, mapArgs(tp.args, tyconTypeParams(tp)))
        case _ =>
          mapOver(tp)
      AnnotatedType(tp0, QualifiedAnnotation(ctx.qualifierSolver.freshVar()))

  def updateTreeInfo(tree: Tree)(using Context) =
    // Pasted from cc/Setup.scala:
    tree match
      case tree: ValOrDefDef =>
        val sym = tree.symbol

        // replace an existing symbol info with inferred types where capture sets of
        // TypeParamRefs and TermParamRefs put in correspondence by BiTypeMaps with the
        // capture sets of the types of the method's parameter symbols and result type.
        def integrateRT(
            info: Type,                     // symbol info to replace
            psymss: List[List[Symbol]],     // the local (type and term) parameter symbols corresponding to `info`
            prevPsymss: List[List[Symbol]], // the local parameter symbols seen previously in reverse order
            prevLambdas: List[LambdaType]   // the outer method and polytypes generated previously in reverse order
          ): Type =
          info match
            case mt: MethodOrPoly =>
              val psyms = psymss.head
              mt.companion(mt.paramNames)(
                mt1 =>
                  if !psyms.exists(_.isUpdatedAfter(preRecheckPhase)) && !mt.isParamDependent && prevLambdas.isEmpty then
                    mt.paramInfos
                  else
                    val subst = SubstParams(psyms :: prevPsymss, mt1 :: prevLambdas)
                    psyms.map(psym => subst(psym.info).asInstanceOf[mt.PInfo]),
                mt1 =>
                  integrateRT(mt.resType, psymss.tail, psyms :: prevPsymss, mt1 :: prevLambdas)
              )
            case info: ExprType =>
              info.derivedExprType(resType =
                integrateRT(info.resType, psymss, prevPsymss, prevLambdas))
            case _ =>
              val restp = tree.tpt.knownType
              if prevLambdas.isEmpty then restp
              else SubstParams(prevPsymss, prevLambdas)(restp)

        if tree.tpt.hasRememberedType && !sym.isConstructor then
          val newInfo = integrateRT(sym.info, sym.paramSymss, Nil, Nil)
            .showing(i"update info $sym: ${sym.info} --> $result")
          if newInfo ne sym.info then
            val completer = new LazyType:
              def complete(denot: SymDenotation)(using Context) =
                denot.info = newInfo
                recheckDef(tree, sym)
            updateInfo(sym, completer)
      case _ => ()

  // Pasted from cc/Setup.scala:
  /** Update info of `sym` for CheckRefinements phase only */
  private def updateInfo(sym: Symbol, info: Type)(using Context) =
    sym.updateInfoBetween(preRecheckPhase, thisPhase, info)

  // Pasted from cc/Setup.scala:
  /** Substitute parameter symbols in `from` to paramRefs in corresponding
   *  method or poly types `to`. We use a single BiTypeMap to do everything.
   *  @param from  a list of lists of type or term parameter symbols of a curried method
   *  @param to    a list of method or poly types corresponding one-to-one to the parameter lists
   */
  private class SubstParams(from: List[List[Symbol]], to: List[LambdaType])(using Context)
  extends DeepTypeMap, BiTypeMap:

    def apply(t: Type): Type = t match
      case t: NamedType =>
        val sym = t.symbol
        def outer(froms: List[List[Symbol]], tos: List[LambdaType]): Type =
          def inner(from: List[Symbol], to: List[ParamRef]): Type =
            if from.isEmpty then outer(froms.tail, tos.tail)
            else if sym eq from.head then to.head
            else inner(from.tail, to.tail)
          if tos.isEmpty then t
          else inner(froms.head, tos.head.paramRefs)
        outer(from, to)
      case _ =>
        mapOver(t)

    def inverse(t: Type): Type = t match
      case t: ParamRef =>
        def recur(from: List[LambdaType], to: List[List[Symbol]]): Type =
          if from.isEmpty then t
          else if t.binder eq from.head then to.head(t.paramNum).namedType
          else recur(from.tail, to.tail)
        recur(to, from)
      case _ =>
        mapOver(t)
  end SubstParams
