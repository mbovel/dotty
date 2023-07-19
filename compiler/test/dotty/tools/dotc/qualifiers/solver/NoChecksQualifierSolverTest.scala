package dotty.tools
package dotc
package qualifiers
package solver

import org.junit.Test
import org.junit.Assert.*

import QualifierExpr.*

final class NoChecksQualifierSolverTest extends QualifierSolverTest(NoChecksQualifierSolver())
