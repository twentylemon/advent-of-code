package org.lemon.advent.year2022

import org.lemon.advent.lib.expressions.*

private object Day21:

  def parseExpression(line: String): (String, Expression[Long]) = line match
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

  def solve(target: String, simplifiedExpression: Expression[Long], knownValue: Long): Long =
    def loop(expression: Expression[Long], knownValue: Long): Long =
      expression match
        case Reference(`target`) => knownValue

        case Add(Literal(n), rhs) => loop(rhs, knownValue - n)
        case Add(lhs, Literal(n)) => loop(lhs, knownValue - n)

        case Subtract(Literal(n), rhs) => loop(rhs, n - knownValue)
        case Subtract(lhs, Literal(n)) => loop(lhs, n - -knownValue)

        case Multiply(Literal(n), rhs) => loop(rhs, knownValue / n)
        case Multiply(lhs, Literal(n)) => loop(lhs, knownValue / n)

        case Divide(Literal(n), rhs) => loop(rhs, n / knownValue)
        case Divide(lhs, Literal(n)) => loop(lhs, n * knownValue)
        case _ => ???
    loop(simplifiedExpression, knownValue)

  def part2(in: String) =
    val monkeys = in.linesIterator.map(parseExpression).toMap
    val human = "humn"
    val context = ExpressionContext(monkeys - human)

    val root = monkeys("root").asInstanceOf[BinaryOperation[Long]]
    val lhs = root.lhs.resolve(context)
    val rhs = root.rhs.resolve(context)

    lhs.map(solve(human, root.rhs.simplify(context), _))
      .orElse(rhs.map(solve(human, root.lhs.simplify(context), _)))
      .get
