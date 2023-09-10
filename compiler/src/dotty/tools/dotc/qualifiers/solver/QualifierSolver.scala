package dotty.tools.dotc.qualifiers
package solver

import QualifierExpr.*

abstract class QualifierSolver:
  def assume(p: QualifierExpr): Unit
  def check(p: QualifierExpr): Boolean
  def push(): Unit
  def pop(): Unit

  def tryImply(p: QualifierExpr, q: QualifierExpr): Boolean =
    push()
    assume(p)
    val res = check(q)
    pop()
    res

  def instantiate(p: QualifierExpr): QualifierExpr

  var maxVarIndex: Int = 0
  def freshVar(): Var =
    val res: Var = Var(maxVarIndex)
    maxVarIndex = maxVarIndex + 1
    res

  var maxRefIndex: Int = 0
  def freshRef(name: String = f"fresh$maxRefIndex"): Ref =
    val res: Ref = Ref(maxRefIndex, name)
    maxRefIndex = maxRefIndex + 1
    res
