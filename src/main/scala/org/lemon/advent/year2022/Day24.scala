package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

private object Day24:

  case class Blizzard(at: Coord, direction: Direction)

  case class Maze(bounds: Area, blizzards: Seq[Set[Coord]], start: Coord, end: Coord):
    def inBounds(coord: Coord) = coord match
      case c if c == start || c == end => true
      case c => bounds.contains(c)

  def move(blizzard: Blizzard, area: Area) =
    blizzard.copy(at = area.wrap(blizzard.at + blizzard.direction))

  def parseBoard(in: String) =
    val lines = in.linesIterator.toSeq
    val start = (lines.head.indexOf('.'), 0)
    val end = (lines.last.indexOf('.'), lines.size - 1)
    val bounds = Area(xRange = 1 to lines.head.size - 2, yRange = 1 to lines.size - 2)
    val initialBlizzards = Coord.gridToMap(in)
      .filter((_, char) => char != '.' && char != '#')
      .map((pos, char) => Blizzard(at = pos, direction = Direction(char)))
      .toSeq

    def tick(blizzards: Seq[Blizzard]) = blizzards.map(move(_, bounds))
    val allBlizzards = Iterator.iterate(initialBlizzards)(tick)
      .take(bounds.width.lcm(bounds.height))
      .map(_.map(_.at).toSet)
      .toIndexedSeq

    Maze(bounds, allBlizzards, start, end)

  case class Step(at: Coord, time: Int)
  def solve(maze: Maze, startingTime: Int = 0) =
    val start = Step(maze.start, startingTime)

    def adjacency(step: Step) =
      val time = step.time + 1
      val blizzards = maze.blizzards(time % maze.blizzards.size)
      (step.at.adjacent :+ step.at)
        .filter(maze.inBounds)
        .filterNot(at => blizzards(at))
        .map(Step(_, time))

    pathFind(adjacency, _.at `manhattan` maze.end, start, _.at == maze.end).get.at

  def part1(in: String) =
    val maze = parseBoard(in)
    solve(maze).time

  def part2(in: String) =
    val maze = parseBoard(in)
    val moveThoseDamnElves = solve(maze)
    val goBackCauseTheyJerks = solve(maze.copy(start = maze.end, end = maze.start), moveThoseDamnElves.time)
    val meetBackUpFfs = solve(maze, goBackCauseTheyJerks.time)
    meetBackUpFfs.time
