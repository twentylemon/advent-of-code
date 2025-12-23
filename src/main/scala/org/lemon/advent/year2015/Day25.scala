package org.lemon.advent.year2015

private object Day25:

  def parse(input: String) = input.trim match
    case s"To continue, please consult the code grid in the manual.  Enter the code at row $r, column $c." =>
      (r.toInt, c.toInt)

  def part1(input: String) =
    val (row, col) = parse(input)
    val diag = row + col - 1
    val idx = diag * (diag - 1) / 2 + col
    20151125 * BigInt(252533).modPow(idx - 1, 33554393) % 33554393
