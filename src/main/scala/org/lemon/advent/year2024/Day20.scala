package org.lemon.advent.year2024

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

private object Day20:

  def parse = Coord.gridToMap

  def adjacency(grid: Map[Coord, Char])(coord: Coord) =
    coord.adjacent.filter(c => grid.getOrElse(c, '#') != '#')

  def goodCheats(grid: Map[Coord, Char], maxCheatDistance: Int) =
    val end = grid.findValue('E').get
    val distances = distanceFrom(adjacency(grid), end)
    distances.keysIterator
      .flatMap(start =>
        start.within(maxCheatDistance, _ `manhattan` _)
          .filter(start.manhattan(_) >= 2)
          .filter(distances.contains)
          .map(to => distances(start) - distances(to) - start.manhattan(to))
      )

  def part1(input: String, minSaving: Int = 100) =
    goodCheats(parse(input), 2).count(_ >= minSaving)

  def part2(input: String, minSaving: Int = 100) =
    goodCheats(parse(input), 20).count(_ >= minSaving)
