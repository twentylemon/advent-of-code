package org.lemon.advent.year2015

private object Day01:

  def part1(input: String) = input.count(_ == '(') - input.count(_ == ')')

  def part2(input: String) =
    input.scanLeft(0)((acc, ch) => acc + (if ch == '(' then 1 else -1)).indexWhere(_ == -1)
