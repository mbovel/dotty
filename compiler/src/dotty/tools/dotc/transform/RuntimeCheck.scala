package dotty.tools.dotc
package transform

import transform.MegaPhase.MiniPhase
import ast.{TreeTypeMap, tpd}
import core.*
import Contexts.*
import dotty.tools.dotc.config.Printers.overload


class RuntimeCheck extends MiniPhase{

  import tpd.*

  override def phaseName: String = RuntimeCheck.name

  override def description: String = RuntimeCheck.description


  override def transformApply(tree: Apply)(using Context): Tree =
    println("***********************************")
    println(tree)
    tree.fun match
      case Ident(name) if name.toString == phaseName =>
        println(tree.args(0))
        println(tree.args(0).tpe)
        tree.args(0) match
          case Ident(name) =>

        //Check that the type of the argument has the correct type with an assertion

      case _ =>
        println("not ident")


    println("***********************************")
    super.transformApply(tree)
}

object RuntimeCheck:
  val name: String = "runtimeChecked"
  val description: String = "runtime check for qualified types"