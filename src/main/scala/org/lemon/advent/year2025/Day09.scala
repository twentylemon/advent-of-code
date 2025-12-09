package org.lemon.advent.year2025

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

import scala.collection.Searching.*
import scala.collection.parallel.CollectionConverters.*

private object Day09:

  def parse(input: String) =
    import org.lemon.advent.lib.parse.*
    input.linesIterator.map(_ match
      case Csv(x, y) => Coord(x.toInt, y.toInt)
    ).toSeq

  def part1(input: String) =
    val coords = parse(input)
    coords.pairs
      .map(_ `bounding` _)
      .map(area => area.width.toLong * area.height.toLong)
      .max

  def part2(input: String) =
    val coords = parse(input)
    val rows = (coords :+ coords.head).sliding(2).collect {
      case Seq(from, to) if from.y == to.y => (from.y, from.bounding(to).xRange.toInterval)
    }.toSeq

    lazy val check: Int => Seq[Interval[Int]] = memoize { x =>
      val crossings = rows.collect { case (y, Interval(from, to)) if from <= x && to >= x => y }.sorted
      if crossings.isEmpty then Seq.empty
      else if crossings.size % 2 == 1 then Seq(Interval(crossings.min, crossings.max))
      else crossings.grouped(2).collect { case Seq(y1, y2) => Interval(y1, y2) }.toSeq
    }

    val corners = rows.flatMap((_, i) => Seq(i.start, i.end)).distinct.sorted.toVector
    def isValid(area: Area): Boolean =
      val lo = corners.search(area.left).insertionPoint
      val hi = corners.search(area.right + 1).insertionPoint
      val xs = corners.slice(lo, hi)

      val allXs = (Vector(area.left) ++ xs :+ area.right)
      val midpoints = allXs.sliding(2).collect { case Seq(a, b) => (a + b) / 2 }.toVector
      val yInterval = area.yRange.toInterval
      (xs ++ midpoints :+ area.left :+ area.right).forall(x => check(x).exists(_.containsSlice(yInterval)))

    coords.pairs.toVector.par
      .map(_ `bounding` _)
      .filter(isValid)
      .map(area => area.width.toLong * area.height.toLong)
      .max
