package org.lemon.advent.lib.ortools

import com.google.ortools.linearsolver.MPVariable

object Var:
  given Conversion[Var, LinearExpr] = LinearExpr(_)

sealed trait Var:
  def underlying: MPVariable
  def name: String = underlying.name()
  def solutionValue: Double = underlying.solutionValue()

case class IntVar(underlying: MPVariable) extends Var:
  def value: Long = solutionValue.round

case class BoolVar(underlying: MPVariable) extends Var:
  def value: Boolean = solutionValue > 0.5

case class NumVar(underlying: MPVariable) extends Var:
  def value: Double = solutionValue
