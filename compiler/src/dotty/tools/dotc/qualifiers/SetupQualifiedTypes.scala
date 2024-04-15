package dotty.tools.dotc.qualifiers

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.Types.Type

import dotty.tools.dotc.transform.PreRecheck

class SetupQualifiedTypes extends PreRecheck, IdentityDenotTransformer
