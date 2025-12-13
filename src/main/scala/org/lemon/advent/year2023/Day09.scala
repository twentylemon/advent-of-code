package org.lemon.advent.year2023

import org.lemon.advent.lib.*

private object Day09:

  def parse(input: String) = input.linesIterator
    .map(_.wsv.map(_.toLong))

  def nextSequenceValue(seq: Seq[Long]) =
    Iterator.iterate(seq)(s => s.zip(s.tail).map((l, r) => r - l))
      .takeWhile(_.size >= 2)
      .map(_.last)
      .sum

  def part1(input: String) = parse(input)
    .map(nextSequenceValue)
    .sum

  def part2(input: String) = parse(input)
    .map(_.reverse)
    .map(nextSequenceValue)
    .sum
