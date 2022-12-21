package org.lemon.advent.year2022

import org.lemon.advent._
import scala.collection.mutable
import optimus.algebra.Var

class Day21Test extends UnitTest {

  sealed trait Expression:
    def resolve(context: ExpressionContext): Option[Long]
    def simplify(context: ExpressionContext): Expression

  case class Literal(n: Long) extends Expression:
    override def resolve(context: ExpressionContext) = Some(n)
    override def simplify(context: ExpressionContext) = this

  case class Reference(name: String) extends Expression:
    override def resolve(context: ExpressionContext) = context.evaluate(context.expressions(name))
    override def simplify(context: ExpressionContext) = context.expressions(name).simplify(context)

  sealed trait BinaryOperation extends Expression:
    def lhs: Expression
    def rhs: Expression
    def op(x: Long, y: Long): Long
    def copy(lhs: Expression, rhs: Expression): BinaryOperation

    override def resolve(context: ExpressionContext) =
      for x <- context.evaluate(lhs); y <- context.evaluate(rhs) yield op(x, y)

    override def simplify(context: ExpressionContext) = resolve(context) match
      case Some(value) => Literal(value)
      case None => copy(lhs = lhs.simplify(context), rhs = rhs.simplify(context))

  case class Add(lhs: Expression, rhs: Expression) extends BinaryOperation:
    override def op(x: Long, y: Long) = x + y

  case class Subtract(lhs: Expression, rhs: Expression) extends BinaryOperation:
    override def op(x: Long, y: Long) = x - y

  case class Multiply(lhs: Expression, rhs: Expression) extends BinaryOperation:
    override def op(x: Long, y: Long) = x * y

  case class Divide(lhs: Expression, rhs: Expression) extends BinaryOperation:
    override def op(x: Long, y: Long) = x / y

  case object Unknown extends Expression:
    override def resolve(context: ExpressionContext) = None
    override def simplify(context: ExpressionContext) = this

  class ExpressionContext(val expressions: Map[String, Expression]):
    private val memory = mutable.Map.empty[Expression, Option[Long]]

    def evaluate(expression: Expression): Option[Long] =
      memory.get(expression) match
        case Some(value) => value
        case None =>
          val result = expression.resolve(this)
          memory += (expression -> result)
          result

  def parseExpression(line: String): (String, Expression) = line match
    case s"$v: $lhs $op $rhs" => (
        v,
        op match
          case "+" => Add(Reference(lhs), Reference(rhs))
          case "-" => Subtract(Reference(lhs), Reference(rhs))
          case "*" => Multiply(Reference(lhs), Reference(rhs))
          case "/" => Divide(Reference(lhs), Reference(rhs))
      )
    case s"$v: $lit" => (v, Literal(lit.toLong))

  def part1(in: Seq[String]) =
    val monkeys = in.map(parseExpression).toMap
    ExpressionContext(monkeys).evaluate(monkeys("root")).get

  def solve(simplifiedExpression: Expression, knownValue: Long): Long =
    simplifiedExpression match
      case Unknown => knownValue

      case Add(Literal(n), rhs) => solve(rhs, knownValue - n)
      case Add(lhs, Literal(n)) => solve(lhs, knownValue - n)

      case Subtract(Literal(n), rhs) => solve(rhs, n - knownValue)
      case Subtract(lhs, Literal(n)) => solve(lhs, n - -knownValue)

      case Multiply(Literal(n), rhs) => solve(rhs, knownValue / n)
      case Multiply(lhs, Literal(n)) => solve(lhs, knownValue / n)

      case Divide(Literal(n), rhs) => solve(rhs, n / knownValue)
      case Divide(lhs, Literal(n)) => solve(lhs, n * knownValue)
      case _ => ???

  def part2(in: Seq[String]) =
    val monkeys = in.map(parseExpression).toMap + ("humn" -> Unknown)

    val context = ExpressionContext(monkeys)
    val root = monkeys("root").asInstanceOf[BinaryOperation]
    val lhs = context.evaluate(root.lhs)
    val rhs = context.evaluate(root.rhs)

    if lhs.isDefined then solve(root.rhs.simplify(context), lhs.get)
    else solve(root.lhs.simplify(context), rhs.get)

  test("part 1 example") {
    val in = """|root: pppw + sjmn
                |dbpl: 5
                |cczh: sllz + lgvd
                |zczc: 2
                |ptdq: humn - dvpt
                |dvpt: 3
                |lfqf: 4
                |humn: 5
                |ljgn: 2
                |sjmn: drzm * dbpl
                |sllz: 4
                |pppw: cczh / lfqf
                |lgvd: ljgn * ptdq
                |drzm: hmdt - zczc
                |hmdt: 32""".stripMargin
    part1(in.linesIterator.toSeq) shouldBe 152
  }

  test("part 1") {
    part1(readLines(file(2022)(21))) shouldBe 168502451381566L
  }

  test("part 2 example") {
    val in = """|root: pppw + sjmn
                |dbpl: 5
                |cczh: sllz + lgvd
                |zczc: 2
                |ptdq: humn - dvpt
                |dvpt: 3
                |lfqf: 4
                |humn: 5
                |ljgn: 2
                |sjmn: drzm * dbpl
                |sllz: 4
                |pppw: cczh / lfqf
                |lgvd: ljgn * ptdq
                |drzm: hmdt - zczc
                |hmdt: 32""".stripMargin
    part2(in.linesIterator.toSeq) shouldBe 301
  }

  test("part 2") {
    part2(readLines(file(2022)(21))) shouldBe 3343167719435L
  }
}
