package dotty.tools.dotc.qualified_types

import scala.compiletime.summonFrom

import dotty.tools.dotc.config.Printers
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.printing.Formatting.ShownDef.Show
import dotty.tools.dotc.reporting

object QualifierTracing:
  inline def trace[T](inline message: String)(inline op: => T)(using Context): T =
    trace[T](
      message,
      res =>
        summonFrom:
          case given Show[T] => i"$res"
          case _             => res.toString()
    )(op)

  inline def trace[T](inline message: String, inline showOp: T => String)(inline op: => T)(using Context): T =
    reporting.trace[T, T](
      removeLineBreaks(message),
      Printers.qualifiedTypes,
      res => removeLineBreaks(showOp(res))
    )(op)

  private def removeLineBreaks(s: String): String =
    s.replaceAll("""[\t ]*(\n[\t ]*)+""", " ")
