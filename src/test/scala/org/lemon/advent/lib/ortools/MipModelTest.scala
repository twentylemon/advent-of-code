package org.lemon.advent.lib.ortools

import org.lemon.advent.*

class MipModelTest extends UnitTest:

  test("maximize picks the best knapsack") {
    val model = MipModel()
    val items = Seq((5, 10), (4, 40), (6, 30), (3, 50))
    val take = items.indices.map(i => model.boolVar(s"take$i"))
    model.subjectTo(items.zip(take).map { case ((weight, _), x) => weight * x }.sumExpr <= 10)
    model.maximize(items.zip(take).map { case ((_, value), x) => value * x }.sumExpr)
    model.solve() shouldBe SolveStatus.Optimal
    model.objective shouldBe 90.0 +- 1e-6
    take.map(_.value) shouldBe Seq(false, true, false, true)
    model.release()
  }

  test("minimize finds the fewest coins") {
    val model = MipModel()
    val threes = model.intVar("threes")
    val fives = model.intVar("fives")
    model.subjectTo(3 * threes + 5 * fives === 17)
    model.minimize(Seq(threes, fives).sumExpr)
    model.solve() shouldBe SolveStatus.Optimal
    model.objective shouldBe 5.0 +- 1e-6
    (threes.value, fives.value) shouldBe (4L, 1L)
    model.release()
  }

  test("a continuous LP solves at a vertex") {
    val model = MipModel()
    val x = model.numVar("x")
    val y = model.numVar("y")
    model.subjectTo(x + 2 * y <= 4)
    model.subjectTo(3 * x + y <= 6)
    model.maximize(x + y)
    model.solve() shouldBe SolveStatus.Optimal
    model.objective shouldBe 2.8 +- 1e-6
    x.value shouldBe 1.6 +- 1e-6
    y.value shouldBe 1.2 +- 1e-6
    model.release()
  }

  test("the objective includes the expression's constant") {
    val model = MipModel()
    val x = model.intVar("x")
    model.subjectTo(x <= 3)
    model.maximize(x + 10)
    model.solve() shouldBe SolveStatus.Optimal
    model.objective shouldBe 13.0 +- 1e-6
    model.release()
  }

  test("contradictory constraints are Infeasible with no objective") {
    val model = MipModel()
    val x = model.intVar("x")
    model.subjectTo(x >= 2)
    model.subjectTo(x <= 1)
    model.getStatus shouldBe SolveStatus.NotSolved
    model.solve() shouldBe SolveStatus.Infeasible
    model.getStatus shouldBe SolveStatus.Infeasible
    an[IllegalStateException] should be thrownBy model.objective
    model.release()
  }

  test("an objective with no bound is Unbounded with no objective") {
    val model = MipModel()
    val x = model.numVar("x", ub = Double.PositiveInfinity)
    model.maximize(x)
    model.solve() shouldBe SolveStatus.Unbounded
    an[IllegalStateException] should be thrownBy model.objective
    model.release()
  }

  test("a time limit doesn't stop a quick solve") {
    val model = MipModel()
    val x = model.intVar("x")
    model.subjectTo(x <= 3)
    model.maximize(x)
    model.setTimeout(10000)
    model.solve() shouldBe SolveStatus.Optimal
    model.objective shouldBe 3.0 +- 1e-6
    model.release()
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
      model.release()
    }
  )

  test("apply looks up a declared variable by name") {
    val model = MipModel()
    val x = model.numVar("x")
    model("x") shouldBe x
    model("x").name shouldBe "x"
    model.release()
  }
