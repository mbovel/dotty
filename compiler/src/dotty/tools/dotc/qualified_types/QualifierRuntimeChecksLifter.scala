package dotty.tools.dotc.qualified_types

import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.Lifter
import dotty.tools.dotc.ast.tpd.{Apply, Block, Tree, Ident, TreeTraverser, TreeOps}
import dotty.tools.dotc.core.Types.SkolemType
import dotty.tools.dotc.core.Contexts.Context
import scala.collection.mutable

final class QualifierRuntimeChecksLifter extends MiniPhase:
  override def phaseName: String = "qualifierRuntimeChecksLifter"

  override def description: String = "Lift arguments that might be used in runtime checks of qualified types"

  private val lifter = new Lifter:
    override def noLift(tree: Tree)(using Context): Boolean = false

  override def transformApply(tree: Apply)(using Context): Tree =
    tree match
      case Apply(fn, args) if shouldLiftApp(fn, args) =>
        //for arg <- args do
        //  arg.tpe.foreachPart: tpe =>
        //    tpe match
        //      case QualifiedType(_, qualifier) =>
        //        qualifier.foreachSubTree:
        //          case tree: Ident if tree.tpe.isInstanceOf[SkolemType] =>
        //            println(arg.show)
        //            println(tree.tpe.show)
        //            println(tree.symbol)
        //          case tree =>
        //            ()
        //      case _ => ()
        val defs = new mutable.ListBuffer[Tree]
        val liftedArgs = lifter.liftArgs(defs, fn.tpe, args)
        val res = Block(defs.toList, Apply(fn, liftedArgs))
        //println(s"QualifierRuntimeChecksLifter: ${res.show}")
        res
      case _ =>
        tree

  private def shouldLiftApp(fn: Tree, args: List[Tree])(using Context): Boolean =
    true
