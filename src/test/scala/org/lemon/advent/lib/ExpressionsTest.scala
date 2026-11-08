package org.lemon.advent.lib

import org.lemon.advent.*
import org.scalacheck.*
import org.scalacheck.Prop.*

class ExpressionsTest extends UnitTest:
  case class Add(lhs: Expression[Long], rhs: Expression[Long]) extends BinaryOperation[Long]:
    override def apply(left: Long, right: Long) = left + right

  case class Negate(operand: Expression[Long]) extends UnaryOperation[Long]:
    override def apply(operand: Long) = -operand

  def expression(depth: Int): Gen[(Expression[Long], Option[Long], Expression[Long])] =
    val leaf = Gen.frequency(
      9 -> Arbitrary.arbitrary[Long].map(n => (Literal(n), Some(n), Literal(n))),
      1 -> Gen.const((Unknown[Long](), None, Unknown[Long]())),
    )
    if depth == 0 then leaf
    else
      val negate = expression(depth - 1).map((expr, value, simplified) =>
        val negated = value.map(-_)
        (Negate(expr), negated, negated.fold[Expression[Long]](Negate(simplified))(Literal(_)))
      )
      val add = Gen.zip(expression(depth - 1), expression(depth - 1)).map {
        case ((lhs, lhsValue, lhsSimplified), (rhs, rhsValue, rhsSimplified)) =>
          val sum = for l <- lhsValue; r <- rhsValue yield l + r
          (Add(lhs, rhs), sum, sum.fold[Expression[Long]](Add(lhsSimplified, rhsSimplified))(Literal(_)))
      }
      Gen.oneOf(leaf, negate, add)

  def chain(length: Int, root: Expression[Long]): Map[String, Expression[Long]] =
    (1 to length).map(i => s"x$i" -> Reference[Long](s"x${i - 1}")).toMap + ("x0" -> root)

  test("resolve is the value of the expression, or None when it contains an Unknown") {
    check(forAll(expression(4), Gen.identifier) { case ((expr, value, _), name) =>
      ExpressionContext(Map(name -> expr)).resolve(name) == value
    })
  }

  test("resolve follows a chain of references") {
    check(forAll(expression(4), Gen.choose(0, 50)) { case ((expr, value, _), length) =>
      ExpressionContext(chain(length, expr)).resolve(s"x$length") == value
    })
  }

  test("resolve of a reference cycle is None") {
    check(forAll(Gen.choose(1, 50), Gen.identifier) { (length, name) =>
      val cycle = (0 until length).map(i => s"$name$i" -> Reference[Long](s"$name${(i + 1) % length}")).toMap
      ExpressionContext(cycle).resolve(s"${name}0").isEmpty
    })
  }

  test("resolve of a missing name is None") {
    check((name: String, other: String, value: Long) =>
      name != other ==> ExpressionContext(Map(other -> Literal(value))).resolve(name).isEmpty
    )
  }

  test("simplify folds every subexpression without an Unknown into a Literal") {
    check(forAll(expression(4), Gen.identifier) { case ((expr, _, simplified), name) =>
      ExpressionContext(Map(name -> expr)).simplify(name) == simplified
    })
  }

  test("simplify replaces a chain of references with the simplified expression") {
    check(forAll(expression(4), Gen.choose(0, 50)) { case ((expr, _, simplified), length) =>
      ExpressionContext(chain(length, expr)).simplify(s"x$length") == simplified
    })
  }
