package org.lemon.advent.year2023

private object Day09:

  def parse(input: String) = input.linesIterator
    .map(_.split(" ").map(_.trim).map(_.toLong).toSeq)

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
