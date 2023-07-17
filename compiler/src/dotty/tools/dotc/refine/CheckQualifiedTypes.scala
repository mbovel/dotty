package dotty.tools
package dotc
package refine

import core.*
import Symbols.*, Contexts.*, Types.*, ContextOps.*, Decorators.*, SymDenotations.*, DenotTransformers.*
import Flags.*
import ast.tpd.*
import Names.Name
import Phases.Phase
import transform.{Recheck, PreRecheck}
import config.{Config, Feature}
import Recheck.*
import annotation.constructorOnly
import dotty.tools.dotc.ast.tpd

class CheckQualifiedTypes extends Recheck:
  override def phaseName = "checkQualifiedTypes"

  override def run(using Context): Unit =
    if Feature.qualifiedTypesEnabled then super.run

  override def newRechecker()(using Context): Rechecker = QualifierRechecker(ctx)
  /** The typechecker pass */
  class QualifierRechecker(@constructorOnly ictx: Context) extends Rechecker(ictx):

    override def recheckTypeTree(tree: tpd.TypeTree)(using Context): Type =
      knownType(tree) match
        case tp: AnnotatedType
          if ctx.phase == Phases.checkQualifiersPhase
            && tp.annot.symbol == defn.QualifiedAnnot =>
          tp.annot.tree.tpe match
            case AppliedType(_, List(arg)) =>
              if arg != tp.parent then
                val msg = em"Malformed qualifier. Expected to qualify ${tp.parent} but was ${arg}."
                report.error(msg, tp.annot.tree.sourcePos)
                ErrorType(msg)
              else knownType(tree)
        case _ => knownType(tree)

    /*
    override def recheckDefDef(tree: DefDef, sym: Symbol)(using Context): Unit =
      tree.termParamss
        .flatten
        .flatMap(param => QualifiedType.unapply(param.tpe))
        .foreach(checkPredicateFormat)

      QualifiedType.unapply(tree.tpe)
        .foreach(checkPredicateFormat)
    */


object CheckQualifiedTypes:
  class Pre extends PreRecheck, IdentityDenotTransformer:
    override def isEnabled(using Context) = true
