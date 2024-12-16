package org.lemon.advent.year2024

import org.lemon.advent.lib.`2d`._
import org.lemon.advent.lib.graph._

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
    val start = grid.find(_._2 == 'S').get._1
    val end = grid.find(_._2 == 'E').get._1
    val facing = Direction.Right
    pathFind(adjacency(grid), (start, facing), _._1 == end).get.distance

  def part2(input: String) =
    val grid = parse(input)
    val start = grid.find(_._2 == 'S').get._1
    val end = grid.find(_._2 == 'E').get._1
    val facing = Direction.Right
    allShortestPaths(adjacency(grid), (start, facing), _._1 == end)
      .flatMap(path => path.path.map(_._1))
      .size
