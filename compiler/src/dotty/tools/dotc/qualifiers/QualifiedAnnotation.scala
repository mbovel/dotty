package dotty.tools
package dotc
package qualifiers

import core.*
import Annotations.*, Types.*, Symbols.*, Contexts.*, ast.tpd
import tpd.{Annotated, Apply, New, TypeApply, TypeTree, Tree}
import scala.collection.mutable
import dotty.tools.dotc.printing.Printer
import dotty.tools.dotc.printing.Texts.{stringToText, Text}

import dotty.tools.dotc.ast.Trees.EmptyTree
import dotty.tools.dotc.qualifiers.QualifierExprs.toClosure

case class QualifiedAnnotation(qualifier: QualifierExpr, qualifierArgType: Type) extends Annotation:
  override def tree(using Context): Tree =
    val qualifierTree = QualifierExprs.toClosure(qualifier, qualifierArgType)
    New(defn.QualifiedAnnot.typeRef.appliedTo(qualifierArgType), List(qualifierTree))

  override def symbol(using Context) = defn.QualifiedAnnot

  override def derivedAnnotation(tree: Tree)(using Context): Annotation = this

  override def toText(printer: Printer): Text =
    "with " ~ printer.toTextQualifierExpr(qualifier)

  override def mapWith(tm: TypeMap)(using Context): Annotation =
    QualifiedAnnotation(qualifier.mapTypes(tm), tm(qualifierArgType))

object QualifiedAnnotation:

  def apply(annot: Annotation, qualifierArgType: Type)(using Context): Annotation =
    annot match
      case _: QualifiedAnnotation => annot
      case _ => QualifiedAnnotation(QualifierExprs.fromClosure(annot.argument(0).get), qualifierArgType)
