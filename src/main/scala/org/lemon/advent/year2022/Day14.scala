package org.lemon.advent.year2022

import org.lemon.advent.lib.`2d`.*

import scala.collection.mutable

private object Day14:

  enum Trickle:
    case Rest(coord: Coord)
    case Fall
  import Trickle.*

  def parseRocks(line: String): Iterator[Area] = line
    .split(" -> ")
    .toSeq
    .map { case s"$x,$y" => Coord(x.toInt, y.toInt) }
    .sliding(2)
    .map { case Seq(c1, c2) => c1.bounding(c2) }

  def trickle(occupied: Coord => Boolean, deep: Coord => Boolean)(source: Coord): Trickle =
    val spot = source.walk(Direction.Down)
      .dropWhile(c => !deep(c) && !occupied(c))
      .next

    if deep(spot) then Fall
    else if !occupied(spot.left) then trickle(occupied, deep)(spot.left)
    else if !occupied(spot.right) then trickle(occupied, deep)(spot.right)
    else Rest(spot.up)

  def part1(input: String): Int =
    val rocks = input.linesIterator.flatMap(parseRocks).toSeq
    val deepest = rocks.map(_.yRange.end).max
    val initialBlocks = rocks.flatten.toSet
    val sand = mutable.Set.from(initialBlocks)
    def occupied = sand.contains
    def deep(coord: Coord) = coord.y > deepest

    var resting = true
    while resting do
      trickle(occupied, deep)(Coord(500, 0)) match
        case Fall => resting = false
        case Rest(coord) => sand += coord
    sand.size - initialBlocks.size

  def part2(input: String): Int =
    val rocks = input.linesIterator.flatMap(parseRocks).toSeq
    val deepest = rocks.map(_.yRange.end).max
    val initialBlocks = rocks.flatten.toSet
    val sand = mutable.Set.from(initialBlocks)
    def occupied(coord: Coord) = coord.y >= deepest + 2 || sand.contains(coord)
    def deep(coord: Coord) = false

    var resting = true
    while resting do
      trickle(occupied, deep)(Coord(500, 0)) match
        case Rest(Coord(500, 0)) => resting = false
        case Rest(coord) => sand += coord
        case Fall => throw AssertionError()
    sand.size - initialBlocks.size + 1
