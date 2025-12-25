package org.lemon.advent.year2016

import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

private object Day13:

  def adjacency(n: Int): Coord => Seq[Coord] = _.adjacent
    .filter(c => c.x >= 0 && c.y >= 0)
    .filter(c =>
      val wall = c.x * c.x + 3 * c.x + 2 * c.x * c.y + c.y + c.y * c.y + n
      java.lang.Integer.bitCount(wall) % 2 == 0
    )

  def part1(input: String, end: Coord = (31, 39)) =
    pathFind(adjacency = adjacency(input.trim.toInt), start = Coord(1, 1), end = end).get.distance

  def part2(input: String) =
    distanceFrom(adjacency = adjacency(input.trim.toInt), end = Coord(1, 1), maxDistance = 50).size
