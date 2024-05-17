package dotty.tools.dotc
package qualifiers

import transform.MegaPhase.MiniPhase
import ast.{TreeTypeMap, tpd}
import core.*
import Contexts.*
import config.Printers.overload
import core.Symbols.defn
import core.Decorators.*


// TODO(Valentin889): Move to `qualifiers` package.
class QualifiedTypesRuntimeChecked extends MiniPhase{

  import tpd.*

  override def phaseName: String = RuntimeCheck.name

  override def description: String = RuntimeCheck.description


  override def transformApply(tree: Apply)(using Context): Tree =

    if (tree.fun.symbol == defn.RuntimeCheckedMethod) then

        //compare tree.tpe and args(0) with args(0).isInstanceOf[TypeTree] use method isInstance inside tpd

        import transform.Recheck.knownType
        import tpd.*
        import Symbols.*
        import NameKinds.UniqueName
        import tasty._
        import Constants.*

        //Create a new val
        val valDef = SyntheticValDef("x".toTermName, tree.args(0))

        // Create the exception type
        val exceptionType = defn.IllegalArgumentExceptionType // Need a better exception

        // Create the New tree node for constructing the exception
        val newException = New(exceptionType, List())

        // Create the Throw tree node for throwing the exception
        val throwException = Throw(newException)

        val valIdent = Ident(valDef.symbol.termRef)

        val condition = valIdent.isInstance(tree.knownType)

        val thenBranch = valIdent.asInstance(tree.knownType)

        val elseBranch = throwException

        val ifStatement = If(condition, thenBranch, elseBranch)

        //import tasty._
        val blockStats = List(valDef)
        val blockExpr = Block(blockStats, ifStatement)

        return blockExpr

    super.transformApply(tree)
}

object RuntimeCheck:
  val name: String = "QualifiedTypesRuntimeChecked"
  val description: String = "runtime check for qualified types"
