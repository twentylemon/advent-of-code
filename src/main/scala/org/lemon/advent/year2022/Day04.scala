package org.lemon.advent.year2022

import org.lemon.advent.lib.*

private object Day04:

  private def parseRange(in: String): Interval[Int] =
    val bounds = in.split('-')
    bounds(0).toInt to bounds(1).toInt

  private def contains(lhs: Interval[Int], rhs: Interval[Int]) =
    lhs.containsSlice(rhs) || rhs.containsSlice(lhs)

  def part1(input: String) = input.linesIterator
    .map(_.csv.map(parseRange))
    .count(r => contains(r(0), r(1)))

  def part2(input: String) = input.linesIterator
    .map(_.csv.map(parseRange))
    .count(r => r(0).intersects(r(1)))
