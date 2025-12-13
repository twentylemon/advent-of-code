package org.lemon.advent.year2024

import org.lemon.advent.lib.*

private object Day01:

  def parse(input: String) = input.linesIterator
    .map(_.wsv)
    .map(p => (p.head.toInt, p.last.toInt))
    .toSeq
    .unzip

  def part1(input: String) =
    val (left, right) = parse(input)
    left.sorted.zip(right.sorted)
      .map((l, r) => (l - r).abs)
      .sum

  def part2(input: String) =
    val (left, right) = parse(input)
    val counts = right.groupBy(r => r).mapValues(_.size)
    left
      .map(l => l * counts.getOrElse(l, 0))
      .sum
