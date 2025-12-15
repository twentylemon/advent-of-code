package org.lemon.advent.year2024

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

private object Day16:

  def parse = Coord.gridToMap

  def adjacency(grid: Map[Coord, Char])(reindeer: (Coord, Direction)): Seq[((Coord, Direction), Int)] =
    val (coord, dir) = reindeer
    val step = coord + dir
    if grid(step) == '#' then
      Seq((coord, dir.turnLeft) -> 1000, (coord, dir.turnRight) -> 1000)
    else
      Seq((coord, dir.turnLeft) -> 1000, (coord, dir.turnRight) -> 1000, (coord + dir, dir) -> 1)

  def part1(input: String) =
    val grid = parse(input)
    val start = grid.findValue('S').get
    val end = grid.findValue('E').get
    val facing = Direction.Right
    pathFind(adjacency(grid), (start, facing), _._1 == end).get.distance

  def part2(input: String) =
    val grid = parse(input)
    val start = grid.findValue('S').get
    val end = grid.findValue('E').get
    val facing = Direction.Right
    allShortestPaths(adjacency(grid), (start, facing), _._1 == end)
      .flatMap(path => path.path.map(_._1))
      .size
