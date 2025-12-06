package org.lemon.advent.year2025

import org.lemon.advent.lib._

private object Day06:

  def parse(input: String) =
    import org.lemon.advent.lib.parse._
    val lines = input.linesIterator.toSeq
    val operands = lines.init.map(_ match
      case Wsv(xs*) => xs.map(_.toLong)
    )
    val operators =
      lines.last match
        case Wsv(ops*) => ops

    (operands, operators)

  def part1(input: String) =
    val (operands, operators) = parse(input)
    operators.zip(operands.transpose)
      .map((op, xs) =>
        op match
          case "+" => xs.sum
          case "*" => xs.product
      )
      .sum

  def split[A](seq: Seq[A])(value: A): Seq[Seq[A]] =
    if seq.isEmpty then Seq.empty
    else
      val (prefix, rest) = seq.span(_ != value)
      prefix +: split(rest.drop(1))(value)

  def part2(input: String) =
    val lines = input.linesIterator.toSeq
    val groups = split(lines.transpose)(List.fill(lines.size)(' '))
    groups.map(problem =>
      val op = problem.head.last
      val operands = problem.map(_.init.mkString.trim.toLong)
      op match
        case '+' => operands.sum
        case '*' => operands.product
    ).sum
