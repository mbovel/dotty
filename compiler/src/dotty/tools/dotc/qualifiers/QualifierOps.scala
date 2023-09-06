package dotty.tools
package dotc
package qualifiers

import core.*
import Types.*, Symbols.*, Contexts.*, ast.tpd.*

extension (tp: Type)
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
