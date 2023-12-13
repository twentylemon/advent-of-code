package org.lemon.advent.year2022

import scala.collection.mutable
import org.lemon.advent._

class Day14Test extends UnitTest {

  type Coord = (Int, Int)
  extension (coord: Coord)
    def x = coord._1
    def y = coord._2
    def up = (coord.x, coord.y - 1)
    def down = (coord.x, coord.y + 1)
    def left = (coord.x - 1, coord.y)
    def right = (coord.x + 1, coord.y)

  type Rock = (Range, Range)
  extension (rock: Rock)
    def xs = rock._1
    def ys = rock._2

  enum Trickle:
    case Rest(coord: Coord)
    case Fall
  import Trickle._

  def parseRocks(line: String): Iterator[Rock] = line
    .split(" -> ")
    .map({ case s"$x,$y" => (x.toInt, y.toInt) })
    .sliding(2)
    .map(pair => (pair.map(_.x).min to pair.map(_.x).max, pair.map(_.y).min to pair.map(_.y).max))

  def trickle(occupied: Coord => Boolean, deep: Coord => Boolean)(source: Coord): Trickle =
    val spot = Iterator.iterate(source)(c => c.down)
      .dropWhile(c => !deep(c) && !occupied(c))
      .next

    if deep(spot) then Fall
    else if !occupied(spot.left) then trickle(occupied, deep)(spot.left)
    else if !occupied(spot.right) then trickle(occupied, deep)(spot.right)
    else Rest(spot.up)

  def part1(in: Seq[String]): Int =
    val rocks = in.flatMap(parseRocks)
    val deepest = rocks.map(_.ys.end).max
    val initialBlocks = rocks.flatMap(r => for x <- r.xs; y <- r.ys yield (x, y)).toSet
    val sand = mutable.Set.from(initialBlocks)
    def occupied = sand.contains
    def deep(coord: Coord) = coord.y > deepest

    var resting = true
    while resting do
      trickle(occupied, deep)((500, 0)) match
        case Fall => resting = false
        case Rest(coord) => sand += coord
    sand.size - initialBlocks.size

  def part2(in: Seq[String]): Int =
    val rocks = in.flatMap(parseRocks)
    val deepest = rocks.map(_.ys.end).max
    val initialBlocks = rocks.flatMap(r => for x <- r.xs; y <- r.ys yield (x, y)).toSet
    val sand = mutable.Set.from(initialBlocks)
    def occupied(coord: Coord) = coord.y >= deepest + 2 || sand.contains(coord)
    def deep(coord: Coord) = false

    var resting = true
    while resting do
      trickle(occupied, deep)((500, 0)) match
        case Rest((500, 0)) => resting = false
        case Rest(coord) => sand += coord
        case Fall => throw AssertionError()
    sand.size - initialBlocks.size + 1

  test("part 1 example") {
    val in = """|498,4 -> 498,6 -> 496,6
                |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

    part1(in.linesIterator.toSeq) shouldBe 24
  }

  test("part 1") {
    part1(readLines(file(2022)(14))) shouldBe 728
  }

  test("part 2 example") {
    val in = """|498,4 -> 498,6 -> 496,6
                |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

    part2(in.linesIterator.toSeq) shouldBe 93
  }

  test("part 2") {
    part2(readLines(file(2022)(14))) shouldBe 27623
  }

}
