package org.lemon.advent.year2024

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

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
    val start = grid.findValue('^').get
    walk(grid, start, Direction('^')).size

  type Paths = Map[Coord, Map[Direction, Coord]]
  def routes(grid: Map[Coord, Char]): Paths =
    val paths =
      for
        (start, ch) <- grid.iterator
        if ch != '#'
        dir <- Direction.values
      yield
        val end = Iterator.iterate(start)(_ + dir)
          .find(coord => !grid.contains(coord) || grid(coord) == '#')
          .get
        start -> (dir -> (if !grid.contains(end) then end else end - dir))
    paths.toSeq.groupBy(_._1).mapValues(_.map(_._2).toMap).toMap

  @annotation.tailrec
  def isLoop(paths: Paths, wall: Coord)(at: Coord, dir: Direction, seen: Set[(Coord, Direction)] = Set()): Boolean =
    if seen.contains((at, dir)) then true
    else if !paths.contains(at) then false
    else
      val dest = paths(at)(dir)
      if dest == wall || at.directionTo(wall).contains(dir) && dest.directionTo(wall).contains(dir.turnAround) then
        isLoop(paths, wall)(wall - dir, dir.turnRight, seen + ((at, dir)))
      else isLoop(paths, wall)(dest, dir.turnRight, seen + ((at, dir)))

  def part2(input: String) =
    val grid = parse(input)
    val paths = routes(grid)
    val start = grid.findValue('^').get
    val dir = Direction('^')
    walk(grid, start, dir).count(isLoop(paths, _)(start, dir))
