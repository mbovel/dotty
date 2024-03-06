package dotty.tools.dotc.qualifiers

import dotty.tools.dotc.transform.PreRecheck
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.SymTransformer
import dotty.tools.dotc.core.SymDenotations.SymDenotation

class SetupQualifiedTypes extends PreRecheck, SymTransformer:
  override def transformSym(symd: SymDenotation)(using Context): SymDenotation = symd
