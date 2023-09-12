package dotty.tools.dotc
package qualifiers

object QualifierLogging:
  inline def log(inline msg: => String): Unit =
    config.Printers.qual.println(msg)
