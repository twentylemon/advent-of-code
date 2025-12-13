package org.lemon.advent.year2024

import org.lemon.advent.lib.*

private object Day07:

  case class Equation(result: Long, operands: Seq[Long])

  def parse(input: String) = input.linesIterator
    .map { case s"$result: $operands" => Equation(result.toLong, operands.wsv.map(_.toLong)) }
    .toSeq

  def check(equation: Equation, ops: Seq[(Long, Long) => Long]): Boolean =
    def doCheck(target: Long, operands: Seq[Long], soFar: Long): Boolean =
      if operands.isEmpty then soFar == target
      else if soFar > target then false
      else ops.exists(f => doCheck(target, operands.tail, f(soFar, operands.head)))
    doCheck(equation.result, equation.operands.tail, equation.operands.head)

  def part1(input: String) =
    val equations = parse(input)
    val ops: Seq[(Long, Long) => Long] = Seq(_ + _, _ * _)
    equations.filter(check(_, ops)).map(_.result).sum

  def part2(input: String) =
    val equations = parse(input)
    val ops: Seq[(Long, Long) => Long] = Seq(_ + _, _ * _, (a, b) => s"$a$b".toLong)
    equations.filter(check(_, ops)).map(_.result).sum
