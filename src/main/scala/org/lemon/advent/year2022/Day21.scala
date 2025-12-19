package org.lemon.advent.year2022

import org.lemon.advent.lib.*

private object Day21:

  type Expr = Expression[Long]
  type BiOp = BinaryOperation[Long]

  case class Add(lhs: Expr, rhs: Expr) extends BiOp:
    override def apply(left: Long, right: Long) = left + right

  case class Subtract(lhs: Expr, rhs: Expr) extends BiOp:
    override def apply(left: Long, right: Long) = left - right

  case class Multiply(lhs: Expr, rhs: Expr) extends BiOp:
    override def apply(left: Long, right: Long) = left * right

  case class Divide(lhs: Expr, rhs: Expr) extends BiOp:
    override def apply(left: Long, right: Long) = left / right

  def parseExpression(line: String): (String, Expr) = line match
    case s"$v: $lhs $op $rhs" => (
        v,
        op match
          case "+" => Add(Reference(lhs), Reference(rhs))
          case "-" => Subtract(Reference(lhs), Reference(rhs))
          case "*" => Multiply(Reference(lhs), Reference(rhs))
          case "/" => Divide(Reference(lhs), Reference(rhs))
      )
    case s"$v: $lit" => (v, Literal(lit.toLong))

  def part1(in: String) =
    val monkeys = in.linesIterator.map(parseExpression).toMap
    ExpressionContext(monkeys).resolve("root").get

  def solve(simplifiedExpression: Expr, knownValue: Long): Long =
    simplifiedExpression match
      case Unknown() => knownValue

      case Add(Literal(n), rhs) => solve(rhs, knownValue - n)
      case Add(lhs, Literal(n)) => solve(lhs, knownValue - n)

      case Subtract(Literal(n), rhs) => solve(rhs, n - knownValue)
      case Subtract(lhs, Literal(n)) => solve(lhs, n - -knownValue)

      case Multiply(Literal(n), rhs) => solve(rhs, knownValue / n)
      case Multiply(lhs, Literal(n)) => solve(lhs, knownValue / n)

      case Divide(Literal(n), rhs) => solve(rhs, n / knownValue)
      case Divide(lhs, Literal(n)) => solve(lhs, n * knownValue)
      case _ => ???

  def part2(in: String) =
    val monkeys = in.linesIterator.map(parseExpression).toMap
    val context = ExpressionContext(monkeys + ("humn" -> Unknown[Long]()))

    val root = monkeys("root").asInstanceOf[BiOp]
    val lhs = root.lhs.resolve(context)
    val rhs = root.rhs.resolve(context)

    lhs.map(solve(root.rhs.simplify(context), _))
      .orElse(rhs.map(solve(root.lhs.simplify(context), _)))
      .get
