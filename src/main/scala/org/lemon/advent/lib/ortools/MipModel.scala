package org.lemon.advent.lib.ortools

import com.google.ortools.Loader
import com.google.ortools.linearsolver.{MPObjective, MPSolver}
import scala.collection.mutable

enum SolverType:
  case CBC, SCIP, SAT

enum SolveStatus:
  case Optimal, Feasible, Infeasible, Unbounded, Abnormal, NotSolved

object MipModel:
  Loader.loadNativeLibraries()

  private def createSolver(solverType: SolverType): MPSolver =
    val solver = MPSolver.createSolver(solverType.toString)
    require(solver != null, s"Could not create solver ${solverType}")
    solver

class MipModel(solverType: SolverType = SolverType.CBC):
  private val solver = MipModel.createSolver(solverType)
  private val variables = mutable.Map.empty[String, Var]
  private var objectiveValue: Option[Double] = None
  private var status: SolveStatus = SolveStatus.NotSolved

  private def declare[V <: Var](name: String, create: => V): V =
    require(!variables.contains(name), s"variable $name already exists")
    val v = create
    variables(name) = v
    v

  def intVar(name: String, lb: Double = 0, ub: Double = Double.MaxValue): IntVar =
    declare(name, IntVar(solver.makeIntVar(lb, ub, name)))

  def boolVar(name: String): BoolVar =
    declare(name, BoolVar(solver.makeBoolVar(name)))

  def numVar(name: String, lb: Double = 0, ub: Double = Double.MaxValue): NumVar =
    declare(name, NumVar(solver.makeNumVar(lb, ub, name)))

  def apply(name: String): Var = variables(name)

  def subjectTo(constraint: LinearConstraint): Unit =
    val c = solver.makeConstraint(constraint.lb, constraint.ub, "")
    constraint.terms.foreach((v, coeff) => c.setCoefficient(v.underlying, coeff))

  private def objectiveOf(expr: LinearExpr): MPObjective =
    val obj = solver.objective()
    expr.terms.foreach((v, coeff) => obj.setCoefficient(v.underlying, coeff))
    obj.setOffset(expr.constant)
    obj

  def maximize(expr: LinearExpr): Unit = objectiveOf(expr).setMaximization()

  def minimize(expr: LinearExpr): Unit = objectiveOf(expr).setMinimization()

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

  def setTimeout(millis: Long): Unit = solver.setTimeLimit(millis)

  def release(): Unit = solver.delete()
