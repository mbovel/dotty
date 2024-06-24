package dotty.tools.dotc
package qualifiers

import transform.MegaPhase.MiniPhase
import ast.{TreeTypeMap, tpd}
import core.*
import Contexts.*
import config.Printers.overload
import core.Symbols.defn
import core.Decorators.*
import transform.Recheck.knownType
import tpd.*
import Symbols.*
import NameKinds.UniqueName
import tasty.*
import Constants.*

class QualifiedTypesRuntimeChecked extends MiniPhase:

  import tpd.*

  override def phaseName: String = RuntimeCheck.name

  override def description: String = RuntimeCheck.description

  override def transformApply(tree: Apply)(using Context): Tree =
    if tree.fun.symbol == defn.RuntimeCheckedMethod then

      return evalOnce(tree.args(0)) { e =>

        // Create the exception type
        val exceptionType = defn.IllegalArgumentExceptionType // Need a better exception

        // Create the New tree node for constructing the exception
        val newException = New(exceptionType, List())

        // Create the Throw tree node for throwing the exception
        val throwException = Throw(newException)

        val valIdent = Ident(e.symbol.termRef)

        val condition = e.isInstance(tree.knownType)

        val thenBranch = e.asInstance(tree.knownType)

        val elseBranch = throwException

        If(condition, thenBranch, elseBranch)

      }

    super.transformApply(tree)

object RuntimeCheck:
  val name: String = "QualifiedTypesRuntimeChecked"
  val description: String = "runtime check for qualified types"
