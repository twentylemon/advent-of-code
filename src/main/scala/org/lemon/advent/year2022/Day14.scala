package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import org.lemon.advent.lib.geometry.*

private object Day14:

  enum Trickle:
    case Rest(coord: Coord)
    case Fall
  import Trickle.*

  def parseRocks(line: String): Iterator[Area] = line
    .split(" -> ")
    .iterator
    .map { case s"$x,$y" => Coord(x.toInt, y.toInt) }
    .sliding2
    .map((c1, c2) => c1.bounding(c2))

  def trickle(occupied: Coord => Boolean, deep: Coord => Boolean)(source: Coord): Trickle =
    val spot = source.walk(Direction.Down).find(c => deep(c) || occupied(c)).get

    if deep(spot) then Fall
    else if !occupied(spot.left) then trickle(occupied, deep)(spot.left)
    else if !occupied(spot.right) then trickle(occupied, deep)(spot.right)
    else Rest(spot.up)

  def part1(input: String): Int =
    val rocks = input.linesIterator.flatMap(parseRocks).toSeq
    val deepest = rocks.map(_.bottom).max
    def deep(coord: Coord) = coord.y > deepest
    Iterator.unfold(rocks.flatten.toSet) { occupied =>
      trickle(occupied, deep)(Coord(500, 0)) match
        case Fall => None
        case Rest(coord) => Some((coord, occupied + coord))
    }.size

  def part2(input: String): Int =
    val rocks = input.linesIterator.flatMap(parseRocks).toSeq
    val deepest = rocks.map(_.bottom).max
    def deep(coord: Coord) = false
    Iterator.unfold(rocks.flatten.toSet) { sand =>
      def occupied(coord: Coord) = coord.y >= deepest + 2 || sand.contains(coord)
      trickle(occupied, deep)(Coord(500, 0)) match
        case Rest(Coord(500, 0)) => None
        case Rest(coord) => Some((coord, sand + coord))
        case Fall => throw AssertionError()
    }.size + 1
