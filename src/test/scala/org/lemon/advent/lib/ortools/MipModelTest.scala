package org.lemon.advent.lib.ortools

import org.lemon.advent.*

class MipModelTest extends UnitTest:

  SolverType.values.foreach(solverType =>
    test(s"maximize picks the best knapsack with $solverType") {
      val model = MipModel(solverType)
      val items = Seq((5, 10), (4, 40), (6, 30), (3, 50))
      val take = items.indices.map(i => model.boolVar(s"take$i"))
      model.subjectTo(items.zip(take).map { case ((weight, _), x) => weight * x }.sumExpr <= 10)
      model.maximize(items.zip(take).map { case ((_, value), x) => value * x }.sumExpr)
      model.solve() shouldBe SolveStatus.Optimal
      model.objective shouldBe 90.0 +- 1e-6
      take.map(_.value) shouldBe Seq(false, true, false, true)
      model.close()
    }

    test(s"minimize finds the fewest coins with $solverType") {
      val model = MipModel(solverType)
      val threes = model.intVar("threes", ub = 17)
      val fives = model.intVar("fives", ub = 17)
      model.subjectTo(3 * threes + 5 * fives === 17)
      model.minimize(Seq(threes, fives).sumExpr)
      model.solve() shouldBe SolveStatus.Optimal
      model.objective shouldBe 5.0 +- 1e-6
      (threes.value, fives.value) shouldBe (4L, 1L)
      model.close()
    }

    test(s"the objective includes the expression's constant with $solverType") {
      val model = MipModel(solverType)
      val x = model.intVar("x", ub = 10)
      model.subjectTo(x <= 3)
      model.maximize(x + 10)
      model.solve() shouldBe SolveStatus.Optimal
      model.objective shouldBe 13.0 +- 1e-6
      model.close()
    }

    test(s"contradictory constraints are Infeasible with no objective with $solverType") {
      val model = MipModel(solverType)
      val x = model.intVar("x", ub = 10)
      model.subjectTo(x >= 2)
      model.subjectTo(x <= 1)
      model.getStatus shouldBe SolveStatus.NotSolved
      model.solve() shouldBe SolveStatus.Infeasible
      model.getStatus shouldBe SolveStatus.Infeasible
      an[IllegalStateException] should be thrownBy model.objective
      model.close()
    }

    test(s"a constant constraint that can't hold is Infeasible with $solverType") {
      val model = MipModel(solverType)
      val x = model.intVar("x", ub = 3)
      model.subjectTo(LinearExpr.Zero + 5 <= 3)
      model.maximize(x)
      model.solve() shouldBe SolveStatus.Infeasible
      model.close()
    }
  )

  Seq(SolverType.CBC, SolverType.SCIP).foreach(solverType =>
    test(s"a continuous LP solves at a vertex with $solverType") {
      val model = MipModel(solverType)
      val x = model.numVar("x")
      val y = model.numVar("y")
      model.subjectTo(x + 2 * y <= 4)
      model.subjectTo(3 * x + y <= 6)
      model.maximize(x + y)
      model.solve() shouldBe SolveStatus.Optimal
      model.objective shouldBe 2.8 +- 1e-6
      x.value shouldBe 1.6 +- 1e-6
      y.value shouldBe 1.2 +- 1e-6
      model.close()
    }
  )

  test("an objective with no bound is Unbounded with no objective") {
    val model = MipModel()
    val x = model.numVar("x", ub = Double.PositiveInfinity)
    model.maximize(x)
    model.solve() shouldBe SolveStatus.Unbounded
    an[IllegalStateException] should be thrownBy model.objective
    model.close()
  }

  test("a time limit doesn't stop a quick solve") {
    val model = MipModel()
    val x = model.intVar("x")
    model.subjectTo(x <= 3)
    model.maximize(x)
    model.setTimeout(10000)
    model.solve() shouldBe SolveStatus.Optimal
    model.objective shouldBe 3.0 +- 1e-6
    model.close()
  }

  Seq[(String, (MipModel, String) => Var)](
    ("intVar", _.intVar(_)),
    ("boolVar", _.boolVar(_)),
    ("numVar", _.numVar(_)),
  ).foreach((kind, declare) =>
    test(s"$kind throws IllegalArgumentException for a name that's already declared") {
      val model = MipModel()
      declare(model, "x")
      an[IllegalArgumentException] should be thrownBy declare(model, "x")
      model.close()
    }
  )

  test("apply looks up a declared variable by name") {
    val model = MipModel()
    val x = model.numVar("x")
    model("x") shouldBe x
    model("x").name shouldBe "x"
    model.close()
  }
