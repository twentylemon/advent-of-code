package org.lemon.advent.year2024

private object Day02:

  def parse(input: String) = input.linesIterator
      .map(_.split("\\s+"))
      .map(_.map(_.toInt).toSeq)
      .toSeq

  def part1(input: String) = parse(input).count(isSafe)

  def isSafe(report: Seq[Int]): Boolean =
    def diffs = report.sliding(2).map { case Seq(a, b) => (b - a).abs }
    val sorted = report.sorted
    (report == sorted || report == sorted.reverse) && diffs.forall(1 to 3 contains _)

  def part2(input: String) =
    parse(input).count(r => r.indices.map(i => r.take(i) ++ r.drop(i + 1)).exists(isSafe))
