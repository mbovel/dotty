package dotty.tools.dotc.qualifiers
package solver

import dotty.tools.dotc.core.Contexts.Context

import QualifierExpr.*

abstract class QualifierSolver:
  def assume(p: QualifierExpr)(using Context): Unit
  def check(p: QualifierExpr)(using Context): Boolean
  def push()(using Context): Unit
  def pop()(using Context): Unit

  def tryImply(p: QualifierExpr, q: QualifierExpr)(using Context): Boolean =
    push()
    assume(p)
    val res = check(q)
    pop()
    res

  def instantiate(p: QualifierExpr)(using Context): QualifierExpr

  var maxVarIndex: Int = 0
  def freshVar(): ApplyVar =
    val res: ApplyVar = ApplyVar(maxVarIndex)
    maxVarIndex = maxVarIndex + 1
    res

  def debug()(using Context): Unit
