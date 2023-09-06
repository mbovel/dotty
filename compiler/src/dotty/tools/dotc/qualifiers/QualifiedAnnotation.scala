package dotty.tools
package dotc
package qualifiers

import core.*
import Annotations.*, Types.*, Symbols.*, Contexts.*, ast.tpd
import scala.collection.mutable
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.printing.Texts.Text

import dotty.tools.dotc.ast.Trees.EmptyTree

case class QualifiedAnnotation(pred: QualifierExpr) extends Annotation:

  // TODO(mbovel): create the right tree
  override def tree(using Context): tpd.Tree = EmptyTree()

  override def symbol(using Context) = defn.QualifiedAnnot

  override def derivedAnnotation(tree: tpd.Tree)(using Context): Annotation = this

  override def toText(printer: Printer): Text = f"with $pred"

  override def show(using Context): String = ???
