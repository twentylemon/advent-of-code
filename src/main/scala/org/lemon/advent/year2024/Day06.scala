package org.lemon.advent.year2024

import org.lemon.advent.lib.`2d`._
import scala.collection.parallel.CollectionConverters._

private object Day06:

  def parse = Coord.gridToMap

  @annotation.tailrec
  def walk(grid: Map[Coord, Char], cameFrom: Coord, dir: Direction, visited: Set[Coord] = Set()): Set[Coord] =
    val goingTo = cameFrom + dir
    if !grid.contains(goingTo) then visited + cameFrom
    else
      grid(goingTo) match
        case '#' => walk(grid, cameFrom, dir.turnRight, visited)
        case _ => walk(grid, goingTo, dir, visited + cameFrom)

  def part1(input: String) =
    val grid = parse(input)
    val start = grid.find(_._2 == '^').get._1
    walk(grid, start, Direction('^')).size

  @annotation.tailrec
  def isLoop(grid: Map[Coord, Char], cameFrom: Coord, dir: Direction, seen: Set[(Coord, Direction)] = Set()): Boolean =
    val goingTo = cameFrom + dir
    if !grid.contains(goingTo) then false
    else if seen.contains((goingTo, dir)) then true
    else
      grid(goingTo) match
        case '#' => isLoop(grid, cameFrom, dir.turnRight, seen + ((cameFrom, dir)))
        case _ => isLoop(grid, goingTo, dir, seen)

  def part2(input: String) =
    val grid = parse(input)
    val start = grid.find(_._2 == '^').get._1
    val dir = Direction('^')
    val path = walk(grid, start, dir)
    path.par.count(c => isLoop(grid + (c -> '#'), start, dir))
