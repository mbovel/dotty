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

class SetupQualifiedTypesTraverser(
    thisPhase: SetupQualifiedTypes,
    recheckDef: (tpd.ValOrDefDef, Symbol) => Context ?=> Type
) extends TreeTraverserWithPreciseImportContexts:

  override def traverse(tree: Tree)(using Context) =
    tree match
      /*
      case tree: DefDef =>
        inContext(localCtx(tree)):
          tree.paramss.foreach(traverse)
          val newResType = tree.tpt match
            case tpt: InferredTypeTree => removeAnnotations(tree.tpt.knownType)
            case tpt => transformExplicitType(tree.tpt.knownType)
          tree.tpt.rememberType(newResType)
          traverse(tree.rhs)
      */
      case tree: ValDef if tree.symbol.is(Flags.Case) =>
        inContext(localCtx(tree)):
          tree.tpt.rememberType(transformExplicitType(tree.tpt.knownType))
          traverse(tree.rhs)
      case tree: InferredTypeTree =>
        tree.rememberType(transformInferredType(tree.knownType))
      case tree: TypeTree =>
        tree.rememberType(transformExplicitType(tree.knownType))
      case _ =>
        traverseChildren(tree)

    postProcess(tree)

  def transformExplicitType(tp: Type)(using Context) =
    // Nothing to do for now :)
    tp

  def transformInferredType(tp: Type)(using Context) =
    addVars(removeAnnotations(tp))

  def postProcess(tree: Tree)(using Context): Unit =
    // Pasted from cc/Setup.scala:
    tree match
      case tree: ValOrDefDef =>
        val sym = tree.symbol

        /** The return type of a constructor instantiated with local type and value
         *  parameters. Constructors have `unit` result type, that's why we can't
         *  get this type by reading the result type tree, and have to construct it
         *  explicitly.
         */
        def constrReturnType(info: Type, psymss: List[List[Symbol]]): Type = info match
          case info: MethodOrPoly =>
            constrReturnType(info.instantiate(psymss.head.map(_.namedType)), psymss.tail)
          case _ =>
            info

        /** The local result type, which is the known type of the result type tree,
         *  with special treatment for constructors.
         */
        def localReturnType =
          if sym.isConstructor then constrReturnType(sym.info, sym.paramSymss)
          else tree.tpt.knownType

        def paramSignatureChanges = tree.match
          case tree: DefDef => tree.paramss.nestedExists:
            case param: ValDef => param.tpt.hasRememberedType
            case param: TypeDef => param.rhs.hasRememberedType
          case _ => false

        def signatureChanges =
          tree.tpt.hasRememberedType && !sym.isConstructor || paramSignatureChanges

        // Replace an existing symbol info with inferred types where capture sets of
        // TypeParamRefs and TermParamRefs put in correspondence by BiTypeMaps with the
        // capture sets of the types of the method's parameter symbols and result type.
        def integrateRT(
            info: Type,                     // symbol info to replace
            psymss: List[List[Symbol]],     // the local (type and term) parameter symbols corresponding to `info`
            resType: Type,                  // the locally computed return type
            prevPsymss: List[List[Symbol]], // the local parameter symbols seen previously in reverse order
            prevLambdas: List[LambdaType]   // the outer method and polytypes generated previously in reverse order
          ): Type =
          info match
            case mt: MethodOrPoly =>
              val psyms = psymss.head
              mt.companion(mt.paramNames)(
                mt1 =>
                  if !paramSignatureChanges && !mt.isParamDependent && prevLambdas.isEmpty then
                    mt.paramInfos
                  else
                    val subst = SubstParams(psyms :: prevPsymss, mt1 :: prevLambdas)
                    psyms.map(psym => subst(psym.nextInfo).asInstanceOf[mt.PInfo]),
                mt1 =>
                  integrateRT(mt.resType, psymss.tail, resType, psyms :: prevPsymss, mt1 :: prevLambdas)
              )
            case info: ExprType =>
              info.derivedExprType(resType =
                integrateRT(info.resType, psymss, resType, prevPsymss, prevLambdas))
            case info =>
              if prevLambdas.isEmpty then resType
              else SubstParams(prevPsymss, prevLambdas)(resType)

        if sym.exists && signatureChanges then
          val newInfo = integrateRT(sym.info, sym.paramSymss, localReturnType, Nil, Nil)
          //  .showing(i"update info $sym: ${sym.info} = $result", capt)
          if newInfo ne sym.info then
            val updatedInfo =
              if sym.isAnonymousFunction
                  || sym.is(Param)
                  || sym.is(ParamAccessor)
                  || sym.isPrimaryConstructor
              then
                // closures are handled specially; the newInfo is constrained from
                // the expected type and only afterwards we recheck the definition
                newInfo
              else new LazyType:
                def complete(denot: SymDenotation)(using Context) =
                  // infos of other methods are determined from their definitions which
                  // are checked on demand
                  assert(ctx.phase == thisPhase.next, i"$sym")
                  //capt.println(i"forcing $sym, printing = ${ctx.mode.is(Mode.Printing)}")
                  //if ctx.mode.is(Mode.Printing) then new Error().printStackTrace()
                  denot.info = newInfo
                  recheckDef(tree, sym)
            updateInfo(sym, updatedInfo)

      case tree: Bind =>
        val sym = tree.symbol
        updateInfo(sym, transformInferredType(sym.info))
      case tree: TypeDef =>
        tree.symbol match
          case cls: ClassSymbol =>
            val cinfo @ ClassInfo(prefix, _, ps, decls, selfInfo) = cls.classInfo
            def innerModule = cls.is(ModuleClass) && !cls.isStatic
            val selfInfo1 =
              // TODO(mbovel): adapt this?
              selfInfo
              //if (selfInfo ne NoType) && !innerModule then
              //  // if selfInfo is explicitly given then use that one, except if
              //  // self info applies to non-static modules, these still need to be inferred
              //  selfInfo
              //else if cls.isPureClass then
              //  // is cls is known to be pure, nothing needs to be added to self type
              //  selfInfo
              //else if !cls.isEffectivelySealed && !cls.baseClassHasExplicitSelfType then
              //  // assume {cap} for completely unconstrained self types of publicly extensible classes
              //  CapturingType(cinfo.selfType, CaptureSet.universal)
              //else
              //  // Infer the self type for the rest, which is all classes without explicit
              //  // self types (to which we also add nested module classes), provided they are
              //  // neither pure, nor are publicily extensible with an unconstrained self type.
              //  CapturingType(cinfo.selfType, CaptureSet.Var(cls))
            val ps1 = inContext(ctx.withOwner(cls)):
              ps.mapConserve(transformExplicitType(_))
            if (selfInfo1 ne selfInfo) || (ps1 ne ps) then
              val newInfo = ClassInfo(prefix, cls, ps1, decls, selfInfo1)
              updateInfo(cls, newInfo)
              //capt.println(i"update class info of $cls with parents $ps selfinfo $selfInfo to $newInfo")
              cls.thisType.asInstanceOf[ThisType].invalidateCaches()
              //if cls.is(ModuleClass) then
              //  // if it's a module, the capture set of the module reference is the capture set of the self type
              //  val modul = cls.sourceModule
              //  updateInfo(modul, CapturingType(modul.info, selfInfo1.asInstanceOf[Type].captureSet))
              //  modul.termRef.invalidateCaches()
          case _ =>
      case _ =>

  // Pasted from cc/Setup.scala:
  /** Update info of `sym` for CheckRefinements phase only */
  private def updateInfo(sym: Symbol, info: Type)(using Context) =
    sym.updateInfo(thisPhase, info)

  // Pasted from cc/Setup.scala:
  extension (sym: Symbol)
    def nextInfo(using Context): Type =
      atPhase(thisPhase.next)(sym.info)

  // Pasted from cc/Setup.scala:
  /** Substitute parameter symbols in `from` to paramRefs in corresponding method or poly types `to`. We use a single BiTypeMap to do everything.
    * @param from
    *   a list of lists of type or term parameter symbols of a curried method
    * @param to
    *   a list of method or poly types corresponding one-to-one to the parameter lists
    */
  private class SubstParams(from: List[List[Symbol]], to: List[LambdaType])(using Context)
      extends DeepTypeMap, BiTypeMap:

    def apply(t: Type): Type = t match
      case t: NamedType =>
        if t.prefix == NoPrefix then
          val sym = t.symbol
          def outer(froms: List[List[Symbol]], tos: List[LambdaType]): Type =
            def inner(from: List[Symbol], to: List[ParamRef]): Type =
              if from.isEmpty then outer(froms.tail, tos.tail)
              else if sym eq from.head then to.head
              else inner(from.tail, to.tail)
            if tos.isEmpty then t
            else inner(froms.head, tos.head.paramRefs)
          outer(from, to)
        else t.derivedSelect(apply(t.prefix))
      case _ =>
        mapOver(t)

    lazy val inverse = new BiTypeMap:
      override def toString = "SubstParams.inverse"
      def apply(t: Type): Type = t match
        case t: ParamRef =>
          def recur(from: List[LambdaType], to: List[List[Symbol]]): Type =
            if from.isEmpty then t
            else if t.binder eq from.head then to.head(t.paramNum).namedType
            else recur(from.tail, to.tail)
          recur(to, from)
        case _ =>
          mapOver(t)
      def inverse = SubstParams.this
  end SubstParams
