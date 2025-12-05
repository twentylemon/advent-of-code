package org.lemon.advent.year2025

import org.lemon.advent.lib._
import cats._
import cats.implicits._
import cats.collections._
import cats.collections.syntax.all._

private object Day05:

  def parse(input: String) =
    import org.lemon.advent.lib.parse._
    input match
      case Chunk(freshies, whoKnows) =>
        val ranges = freshies.linesIterator.map(_ match
          case s"$start-$end" => start.toLong toIncl end.toLong
        ).toSeq
        val ids = whoKnows.linesIterator.map(_.toLong).toSeq
        (ranges, ids)

  def part1(input: String) =
    val (ranges, ids) = parse(input)
    ids.count(id => ranges.exists(_.contains(id)))

  def part2(input: String) =
    val (ranges, _) = parse(input)
    val coverage = ranges.foldLeft(Diet.empty[Long])((diet, range) => diet + range)
    coverage.toIterator.map(range => range.end - range.start + 1).sum
