package org.lemon.advent.year2024

import org.lemon.advent.lib._
import org.lemon.advent.lib.`2d`._
import org.lemon.advent.lib.graph._

private object Day20:

  def parse = Coord.gridToMap

  def findCheats(grid: Map[Coord, Char]): Seq[(Coord, Coord)] =
    def isTrack(c: Char) = c == '.' || c == 'S' || c == 'E'
    def get(coord: Coord) =
      val c = grid.getOrElse(coord, ' ')
      if isTrack(c) then '.' else c

    grid.iterator
      .filter((_, c) => isTrack(c))
      .flatMap((coord, _) =>
        for
          d <- Direction.values
          if get(coord.shift(d, 1)) == '#'
          if get(coord.shift(d, 2)) == '.'
        yield (coord, coord.shift(d, 2))
      )
      .toSeq.sorted

  def adjacency(grid: Map[Coord, Char], cheat: (Coord, Coord))(coord: Coord) =
    val neigh = coord.adjacent.filter(c => grid.getOrElse(c, '#') != '#').map((_, 1))
    if coord == cheat._1 then (cheat._2, 2) +: neigh else neigh

  def part1(input: String) =
    val grid = parse(input)
    val start = grid.find(_._2 == 'S').get._1
    val end = grid.find(_._2 == 'E').get._1
    val noCheating = pathFind(adjacency(grid, ((-1, -1), (-1, -1))), start, end).get.distance

    val cheats = findCheats(grid)
    cheats
      .map(cheat => pathFind(adjacency(grid, cheat), start, end).get.distance)
      .map(noCheating - _)
      .count(_ >= 100)

  def part2(input: String) =
    0
