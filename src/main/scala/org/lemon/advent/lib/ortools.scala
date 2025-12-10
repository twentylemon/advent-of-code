package org.lemon.advent.lib

import com.google.ortools.Loader
import com.google.ortools.linearsolver.{MPSolver, MPVariable, MPConstraint, MPObjective}
import scala.collection.mutable

enum SolverType:
  case CBC, SCIP, SAT

enum SolveStatus:
  case Optimal, Feasible, Infeasible, Unbounded, Abnormal, NotSolved

class MipModel(name: String = "mip", solverType: SolverType = SolverType.CBC):
  Loader.loadNativeLibraries()
  private val solver = MPSolver.createSolver(solverType.toString)
  require(solver != null, s"Could not create solver ${solverType}")
  private val variables = mutable.Map.empty[String, MPVariable]
  private var objectiveValue: Option[Double] = None
  private var status: SolveStatus = SolveStatus.NotSolved

  def intVar(name: String, lb: Double = 0, ub: Double = Double.MaxValue): IntVar =
    val v = solver.makeIntVar(lb, ub, name)
    variables(name) = v
    IntVar(v)

  def boolVar(name: String): BoolVar =
    val v = solver.makeBoolVar(name)
    variables(name) = v
    BoolVar(v)

  def numVar(name: String, lb: Double = 0, ub: Double = Double.MaxValue): NumVar =
    val v = solver.makeNumVar(lb, ub, name)
    variables(name) = v
    NumVar(v)

  def subjectTo(constraint: LinearConstraint): Unit =
    val c = solver.makeConstraint(constraint.lb, constraint.ub, "")
    constraint.terms.foreach((v, coeff) => c.setCoefficient(v.underlying, coeff))

  def maximize(expr: LinearExpr): Unit =
    val obj = solver.objective()
    expr.terms.foreach((v, coeff) => obj.setCoefficient(v.underlying, coeff))
    obj.setOffset(expr.constant)
    obj.setMaximization()

  def minimize(expr: LinearExpr): Unit =
    val obj = solver.objective()
    expr.terms.foreach((v, coeff) => obj.setCoefficient(v.underlying, coeff))
    obj.setOffset(expr.constant)
    obj.setMinimization()

  def solve(): SolveStatus =
    val result = solver.solve()
    status =
      result match
        case MPSolver.ResultStatus.OPTIMAL => SolveStatus.Optimal
        case MPSolver.ResultStatus.FEASIBLE => SolveStatus.Feasible
        case MPSolver.ResultStatus.INFEASIBLE => SolveStatus.Infeasible
        case MPSolver.ResultStatus.UNBOUNDED => SolveStatus.Unbounded
        case MPSolver.ResultStatus.ABNORMAL => SolveStatus.Abnormal
        case _ => SolveStatus.NotSolved
    if status == SolveStatus.Optimal || status == SolveStatus.Feasible then
      objectiveValue = Some(solver.objective().value())
    status

  def objective: Double = objectiveValue.getOrElse(throw IllegalStateException("No solution found"))

  def getStatus: SolveStatus = status

  def setTimeout(seconds: Double): Unit = solver.setTimeLimit((seconds * 1000).toLong)

  def release(): Unit = solver.delete()

sealed trait Var:
  def underlying: MPVariable
  def name: String = underlying.name()
  def solutionValue: Double = underlying.solutionValue()

object Var:
  given Conversion[Var, LinearExpr] = LinearExpr(_)

case class IntVar(underlying: MPVariable) extends Var:
  def value: Long = solutionValue.round

object IntVar:
  given Conversion[IntVar, LinearExpr] = LinearExpr(_)

case class BoolVar(underlying: MPVariable) extends Var:
  def value: Boolean = solutionValue > 0.5

object BoolVar:
  given Conversion[BoolVar, LinearExpr] = LinearExpr(_)

case class NumVar(underlying: MPVariable) extends Var:
  def value: Double = solutionValue

object NumVar:
  given Conversion[NumVar, LinearExpr] = LinearExpr(_)

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

object LinearExpr:
  val Zero: LinearExpr = LinearExpr(Map.empty, 0)

  def apply(v: Var): LinearExpr = LinearExpr(Map(v -> 1.0), 0)

  given Conversion[Int, LinearExpr] = n => LinearExpr(Map.empty, n.toDouble)
  given Conversion[Long, LinearExpr] = n => LinearExpr(Map.empty, n.toDouble)
  given Conversion[Double, LinearExpr] = n => LinearExpr(Map.empty, n)

case class LinearConstraint(terms: Map[Var, Double], lb: Double, ub: Double)

extension [N: Numeric](n: N)
  def *[V <: Var](v: V): LinearExpr = LinearExpr(Map(v -> summon[Numeric[N]].toDouble(n)), 0)
  def *(e: LinearExpr): LinearExpr = e * summon[Numeric[N]].toDouble(n)

extension (vars: Iterable[Var])
  def sumExpr: LinearExpr = vars.foldLeft(LinearExpr.Zero)((acc, v) => acc + LinearExpr(v))

extension (exprs: Iterable[LinearExpr])
  @scala.annotation.targetName("sumLinearExpr")
  def sumExpr: LinearExpr = exprs.foldLeft(LinearExpr.Zero)(_ + _)
