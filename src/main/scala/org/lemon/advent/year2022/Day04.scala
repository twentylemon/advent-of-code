package org.lemon.advent.year2022

import org.lemon.advent.lib.Interval

private object Day04:

  private def parseRange(in: String) =
    val bounds = in.split('-')
    bounds(0).toInt to bounds(1).toInt

  private def contains(lhs: Range, rhs: Range) =
    lhs.containsSlice(rhs) || rhs.containsSlice(lhs)

  def part1(input: String) = input.linesIterator
    .map(_.split(',').map(parseRange))
    .count(r => contains(r(0), r(1)))

  def part2(input: String) = input.linesIterator
    .map(_.split(',').map(parseRange).map(Interval(_)))
    .count(r => r(0).intersects(r(1)))
