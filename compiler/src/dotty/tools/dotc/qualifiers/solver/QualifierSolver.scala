package dotty.tools.dotc.qualifiers
package solver

import dotty.tools.dotc.core.Contexts.Context

import QualifierExpr.*

abstract class QualifierSolver(using Context):
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
  def freshVar(): ApplyVar =
    val res: ApplyVar = ApplyVar(maxVarIndex)
    maxVarIndex = maxVarIndex + 1
    res

  def debug(): Unit
