package org.lemon.advent.year2025

import org.lemon.advent.lib._

private object Day02:

  def parse(input: String) =
    input.linesIterator
      .flatMap(_.split(","))
      .map(_ match
        case s"$start-$end" => start.toLong to end.toLong
      )
      .toSeq

  def isDoubled(x: Long): Boolean =
    val s = x.toString
    val n = s.length
    n % 2 == 0 && s.take(n / 2) == s.drop(n / 2)

  def part1(input: String) =
    val ranges = parse(input)
    ranges.flatMap(range => range.filter(isDoubled)).sum

  def isRepeatedPattern(x: Long): Boolean =
    val s = x.toString
    val n = s.length
    (1 to n / 2)
      .filter(n % _ == 0)
      .map(s.take(_))
      .exists(pattern => s.grouped(pattern.length).forall(_ == pattern))

  def part2(input: String) =
    val ranges = parse(input)
    ranges.flatMap(range => range.filter(isRepeatedPattern)).sum
