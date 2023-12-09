package org.lemon.advent.year2023

private object Day09:

  def parse(input: String) = input.linesIterator
    .map(_.split(" ").map(_.trim).map(_.toLong).toSeq)

  def nextSequenceValue(seq: Seq[Long]) =
    Iterator.iterate(seq)(_.sliding(2).map { case Seq(l, r) => r - l }.toSeq)
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
