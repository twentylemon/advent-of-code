package org.lemon.advent.year2022

private object Day06:

  private def firstDistinctGroup(in: String, window: Int) = in
    .sliding(window)
    .zipWithIndex
    .find((s, _) => s.distinct.length == window)
    .get._2 + window

  def part1(input: String) = firstDistinctGroup(input, 4)

  def part2(input: String) = firstDistinctGroup(input, 14)
