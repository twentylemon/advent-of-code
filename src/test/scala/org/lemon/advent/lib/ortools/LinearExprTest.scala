package org.lemon.advent.lib.ortools

import org.lemon.advent.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*

class LinearExprTest extends UnitTest:
  val model = MipModel()
  val x = model.numVar("x")
  val y = model.numVar("y")
  val z = model.numVar("z")

  test("an Int converts to a constant expression") {
    check((n: Int) => (n: LinearExpr) == LinearExpr(Map.empty, n.toDouble))
  }

  test("a Long converts to a constant expression") {
    check((n: Long) => (n: LinearExpr) == LinearExpr(Map.empty, n.toDouble))
  }

  test("a Double converts to a constant expression") {
    check((d: Double) => !d.isNaN ==> ((d: LinearExpr) == LinearExpr(Map.empty, d)))
  }

  test("+ adds the coefficients of each variable and the constants") {
    check((a: Int, b: Int, c: Int, d: Int, e: Int) =>
      (a * x + b * y + c) + (d * x + e) == LinearExpr(Map(x -> (a.toDouble + d), y -> b.toDouble), c.toDouble + e)
    )
  }

  test("- subtracts the coefficients of each variable and the constants") {
    check((a: Int, b: Int, c: Int, d: Int, e: Int) =>
      (a * x + b * y + c) - (d * x + e) == LinearExpr(Map(x -> (a.toDouble - d), y -> b.toDouble), c.toDouble - e)
    )
  }

  test("* scales every coefficient and the constant") {
    check((a: Int, b: Int, c: Int, n: Int) =>
      n * (a * x + b * y + c) == LinearExpr(Map(x -> a.toDouble * n, y -> b.toDouble * n), c.toDouble * n)
    )
  }

  test("unary - negates every coefficient and the constant") {
    check((a: Int, b: Int, c: Int) =>
      -(a * x + b * y + c) == LinearExpr(Map(x -> -a.toDouble, y -> -b.toDouble), -c.toDouble)
    )
  }

  test("<= between expressions keeps the variables on the left and the constants on the right") {
    check((a: Int, b: Int, c: Int, d: Int) =>
      ((a * x + c) <= (b * y + d)) ==
        LinearConstraint(Map(x -> a.toDouble, y -> -b.toDouble), Double.NegativeInfinity, d.toDouble - c)
    )
  }

  test(">= between expressions keeps the variables on the left and the constants on the right") {
    check((a: Int, b: Int, c: Int, d: Int) =>
      ((a * x + c) >= (b * y + d)) ==
        LinearConstraint(Map(x -> a.toDouble, y -> -b.toDouble), d.toDouble - c, Double.PositiveInfinity)
    )
  }

  test("=== between expressions keeps the variables on the left and the constants on the right") {
    check((a: Int, b: Int, c: Int, d: Int) =>
      ((a * x + c) === (b * y + d)) ==
        LinearConstraint(Map(x -> a.toDouble, y -> -b.toDouble), d.toDouble - c, d.toDouble - c)
    )
  }

  val varGen = Gen.oneOf(x, y, z)
  val exprGen =
    for
      a <- arbitrary[Int]
      b <- arbitrary[Int]
      c <- arbitrary[Int]
    yield a * x + b * y + c

  test("sumExpr of no variables is zero") {
    Seq.empty[Var].sumExpr shouldBe LinearExpr.Zero
  }

  test("sumExpr of one variable is that variable") {
    check(forAll(varGen)(v => Seq(v).sumExpr == LinearExpr(v)))
  }

  test("sumExpr of concatenated variables is the sum of their sumExprs") {
    check(forAll(Gen.listOf(varGen), Gen.listOf(varGen))((xs, ys) => (xs ++ ys).sumExpr == xs.sumExpr + ys.sumExpr))
  }

  test("sumExpr of no expressions is zero") {
    Seq.empty[LinearExpr].sumExpr shouldBe LinearExpr.Zero
  }

  test("sumExpr of one expression is that expression") {
    check(forAll(exprGen)(e => Seq(e).sumExpr == e))
  }

  test("sumExpr of concatenated expressions is the sum of their sumExprs") {
    check(forAll(Gen.listOf(exprGen), Gen.listOf(exprGen))((xs, ys) => (xs ++ ys).sumExpr == xs.sumExpr + ys.sumExpr))
  }
