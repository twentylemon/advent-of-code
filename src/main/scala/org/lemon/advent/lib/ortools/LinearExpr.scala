package org.lemon.advent.lib.ortools

object LinearExpr:
  val Zero: LinearExpr = LinearExpr(Map.empty, 0)

  def apply(v: Var): LinearExpr = LinearExpr(Map(v -> 1.0), 0)

  given Conversion[Int, LinearExpr] = n => LinearExpr(Map.empty, n.toDouble)
  given Conversion[Long, LinearExpr] = n => LinearExpr(Map.empty, n.toDouble)
  given Conversion[Double, LinearExpr] = n => LinearExpr(Map.empty, n)

case class LinearExpr(terms: Map[Var, Double], constant: Double = 0):
  def +(other: LinearExpr): LinearExpr =
    val merged = (terms.toSeq ++ other.terms.toSeq).groupMapReduce(_._1)(_._2)(_ + _)
    LinearExpr(merged, constant + other.constant)

  def -(other: LinearExpr): LinearExpr = this + -other
  def *(scalar: Double): LinearExpr = LinearExpr(terms.view.mapValues(_ * scalar).toMap, constant * scalar)
  def unary_- : LinearExpr = this * -1

  def <=(rhs: Double): LinearConstraint = LinearConstraint(terms, Double.NegativeInfinity, rhs - constant)
  def >=(rhs: Double): LinearConstraint = LinearConstraint(terms, rhs - constant, Double.PositiveInfinity)
  def ===(rhs: Double): LinearConstraint = LinearConstraint(terms, rhs - constant, rhs - constant)
  def <=(rhs: LinearExpr): LinearConstraint = (this - rhs) <= 0
  def >=(rhs: LinearExpr): LinearConstraint = (this - rhs) >= 0
  def ===(rhs: LinearExpr): LinearConstraint = (this - rhs) === 0

case class LinearConstraint(terms: Map[Var, Double], lb: Double, ub: Double)

extension [N: Numeric](n: N)
  def *[V <: Var](v: V): LinearExpr = LinearExpr(Map(v -> Numeric[N].toDouble(n)), 0)
  def *(e: LinearExpr): LinearExpr = e * Numeric[N].toDouble(n)

extension (vars: Iterable[Var])
  def sumExpr: LinearExpr = vars.foldLeft(LinearExpr.Zero)((acc, v) => acc + LinearExpr(v))

extension (exprs: Iterable[LinearExpr])
  @scala.annotation.targetName("sumLinearExpr")
  def sumExpr: LinearExpr = exprs.foldLeft(LinearExpr.Zero)(_ + _)
