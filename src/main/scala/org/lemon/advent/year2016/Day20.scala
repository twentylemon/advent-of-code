package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day20:

  def parse(input: String) = input.linesIterator.map {
    case s"$lo-$hi" => Interval(lo.toLong, hi.toLong)
  }.toSeq

  def part1(input: String) =
    val ineligible = parse(input)
    val diet = Diet(ineligible)
    diet.intervalsIterator.next.end + 1

  def part2(input: String, total: Long = math.pow(2, 32).toLong) =
    val ineligible = parse(input)
    val diet = Diet(ineligible)
    total - diet.size
