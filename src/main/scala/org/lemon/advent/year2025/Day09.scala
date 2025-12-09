package org.lemon.advent.year2025

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day09:

  def parse(input: String) =
    import org.lemon.advent.lib.parse.*
    input.linesIterator.map(_ match
      case Csv(x, y) => Point(x.toLong, y.toLong)
    ).toSeq

  def part1(input: String) =
    val coords = parse(input)
    coords.pairs
      .map(_ `bounding` _)
      .map(_.size)
      .max

  def part2(input: String) =
    val coords = parse(input)
    val segments = (coords.last +: coords).zip(coords).map(_ `bounding` _)
    coords.pairs
      .map(_ `bounding` _)
      .filter(area =>
        val greens = area.contract(1)
        greens.isEmpty || !segments.exists(_.intersects(greens))
      )
      .map(_.size)
      .max
