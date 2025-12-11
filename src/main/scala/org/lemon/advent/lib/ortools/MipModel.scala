package org.lemon.advent.lib.ortools

import com.google.ortools.Loader
import com.google.ortools.linearsolver.MPSolver
import scala.collection.mutable

enum SolverType:
  case CBC, SCIP, SAT

enum SolveStatus:
  case Optimal, Feasible, Infeasible, Unbounded, Abnormal, NotSolved

class MipModel(name: String = "mip", solverType: SolverType = SolverType.CBC):
  Loader.loadNativeLibraries()
  private val solver = MPSolver.createSolver(solverType.toString)
  require(solver != null, s"Could not create solver ${solverType}")
  private val variables = mutable.Map.empty[String, Var]
  private var objectiveValue: Option[Double] = None
  private var status: SolveStatus = SolveStatus.NotSolved

  def intVar(name: String, lb: Double = 0, ub: Double = Double.MaxValue): IntVar =
    require(!variables.contains(name), s"variable $name already exists")
    variables.getOrElseUpdate(name, IntVar(solver.makeIntVar(lb, ub, name))).asInstanceOf[IntVar]

  def boolVar(name: String): BoolVar =
    require(!variables.contains(name), s"variable $name already exists")
    variables.getOrElseUpdate(name, BoolVar(solver.makeBoolVar(name))).asInstanceOf[BoolVar]

  def numVar(name: String, lb: Double = 0, ub: Double = Double.MaxValue): NumVar =
    require(!variables.contains(name), s"variable $name already exists")
    variables.getOrElseUpdate(name, NumVar(solver.makeNumVar(lb, ub, name))).asInstanceOf[NumVar]

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

  def setTimeout(millis: Long): Unit = solver.setTimeLimit(millis)

  def release(): Unit = solver.delete()
