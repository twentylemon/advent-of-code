package org.lemon.advent.year2024

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

private object Day20:

  def parse = Coord.gridToMap

  def adjacency(grid: Map[Coord, Char])(coord: Coord) =
    coord.adjacent.filter(c => grid.getOrElse(c, '#') != '#')

  def manhattans(coord: Coord, maxDist: Int) =
    def manhattan(dist: Int) = (1 to dist).iterator
      .flatMap(d =>
        val inv = dist - d
        Seq(
          Coord(coord.x + d, coord.y + inv),
          Coord(coord.x + inv, coord.y - d),
          Coord(coord.x - d, coord.y - inv),
          Coord(coord.x - inv, coord.y + d)
        )
      )
    (2 to maxDist).iterator.flatMap(manhattan)

  def goodCheats(grid: Map[Coord, Char], maxCheatDistance: Int) =
    val end = grid.findValue('E').get
    val distances = distanceFrom(adjacency(grid), end)
    distances.keysIterator
      .flatMap(start =>
        manhattans(start, maxCheatDistance)
          .filter(distances.contains)
          .map(to => distances(start) - distances(to) - start.manhattan(to))
      )

  def part1(input: String, minSaving: Int = 100) =
    goodCheats(parse(input), 2).count(_ >= minSaving)

  def part2(input: String, minSaving: Int = 100) =
    goodCheats(parse(input), 20).count(_ >= minSaving)
