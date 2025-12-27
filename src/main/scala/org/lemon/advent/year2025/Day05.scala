package org.lemon.advent.year2025

import org.lemon.advent.lib.*

private object Day05:

  def parse(input: String) =
    val Seq(freshies, whoKnows) = input.chunks
    val ranges = freshies.linesIterator.map {
      case s"$start-$end" => (start.toLong, end.toLong)
    }.toSeq
    val ids = whoKnows.linesIterator.map(_.toLong).toSeq
    (ranges, ids)

  def part1(input: String) =
    val (ranges, ids) = parse(input)
    ids.count(id => ranges.map(_.toInterval).exists(_.contains(id)))

  def part2(input: String) =
    val (ranges, _) = parse(input)
    Diet(ranges).size
