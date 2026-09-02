package org.lemon.advent.year2024

private object Day03:

  def part1(input: String) = raw"mul\((\d{1,3}),(\d{1,3})\)".r
    .findAllMatchIn(input)
    .map(m => m.group(1).toInt * m.group(2).toInt)
    .sum

  def part2(input: String) =
    val matches = raw"mul\((\d{1,3}),(\d{1,3})\)|(do\(\))|(don't\(\))".r
      .findAllIn(input)
      .toSeq
    matches.zipWithIndex
      .filter(_._1.startsWith("mul"))
      .filter((_, i) => matches.lastIndexOf("do()", i) >= matches.lastIndexOf("don't()", i))
      .map(_._1 match
        case s"mul($a,$b)" => a.toInt * b.toInt)
      .sum
