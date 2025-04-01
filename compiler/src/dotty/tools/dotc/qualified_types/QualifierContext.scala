package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.util.Property
import dotty.tools.dotc.ast.tpd.{ref, singleton, Tree, Bind,RefTree, EmptyTree, CaseClassApply, CaseClassUnApply, UnApply, Typed, Ident, TypeApply, Select, given}
import dotty.tools.dotc.core.Contexts.{ctx, Context}
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Types.{NamedType, TypeVar, Type, SingletonType, AppliedType, NoPrefix}
import dotty.tools.dotc.config.Feature

import QualifierTracing.trace

object QualifierContext:
  /** A key to be used in a context property that tracks the current qualifier context */
  private val key = new Property.Key[QualifierContext]

  def caseContext(scrutinee: Tree, pattern: Tree)(using Context): Context =
    val value = patternValue(pattern)
    if !Feature.qualifiedTypesEnabled || value.isEmpty then
      ctx
    else
      val assumption = scrutinee.equal(value)
      trace(i"Add assumption $assumption to qualifier context")(())
      val prev = ctx.property(key).getOrElse(QualifierContext(Nil))
      ctx.withProperty(key, Some(prev.addAssumption(assumption)))

  private def patternValue(pattern: Tree)(using Context): Tree =
    pattern match
      case Bind(name, pat) =>
        val patValue = patternValue(pat)
        if patValue.isEmpty then
          ref(pattern.symbol)
        else
          patValue

      case Ident(nme.WILDCARD) =>
        EmptyTree

      case pattern: RefTree =>
        pattern

      case Typed(CaseClassUnApply(classSym, pats), tpt) =>
        val applySym = classSym.companionModule.findMember(nme.apply, NoPrefix, required = Flags.Synthetic, excluded = Flags.EmptyFlags).symbol
        assert(applySym.exists, s"Cannot find apply method for case class $classSym")
        val applyTree = ref(applySym)
        val applyTree1 = tpt.tpe match
          case AppliedType(_, args) =>
            applyTree.appliedToTypes(args)
          case _ =>
            applyTree
        applyTree1.appliedToTermArgs(pats.map(patternValue))

      case _ =>
        EmptyTree

private[qualified_types] case class QualifierContext(val assumptions: List[Tree]):
  def addAssumption(assumption: Tree): QualifierContext =
    copy(assumptions = assumption :: assumptions)


