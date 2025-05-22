package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.ast.tpd.{Apply, TypeApply, If,Select, New, Block, Tree, Throw, evalOnce, given}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.{defn}
import scala.collection.mutable

final class QualifierRuntimeChecks extends MiniPhase:
  override def phaseName: String = "qualifierRuntimeChecks"

  override def description: String = "Compiles `runtimeChecked` calls for qualified types"

  override def transformTypeApply(tree: TypeApply)(using Context): Tree =
    if false then
      val TypeApply(Select(arg, _), List(tpt)) = tree: @unchecked
      val tp = tpt.tpe
      evalOnce(arg): e =>
        If(
          e.isInstance(tp),
          e.asInstance(tp),
          Throw(New(defn.IllegalArgumentExceptionType, List()))
        )
    else
      tree
