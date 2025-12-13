package org.lemon.advent.year2022

import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

import scala.collection.mutable

private object Day12:

  type Maze = Seq[Seq[Char]]
  extension (maze: Maze)
    def apply(coord: Coord): Char =
      val c = maze(coord.y)(coord.x)
      c match
        case 'S' => 'a'
        case 'E' => 'z'
        case _ => c
    def findFirstOf(c: Char) = (for
      y <- maze.indices
      x <- maze(y).indices
      if maze(y)(x) == c
    yield (x, y)).head
    def start = maze.findFirstOf('S')
    def end = maze.findFirstOf('E')

  def parseMaze(lines: Seq[String]) = lines.map(_.toSeq)

  def part1(input: Seq[String]) =
    val maze = parseMaze(input)
    val adjacency = (coord: Coord) =>
      coord.adjacent.filter(maze.hasCoord).filter(neigh => maze(neigh) - maze(coord) < 2)
    pathFind(adjacency, maze.start, maze.end).map(_.distance).get

  def part2(input: Seq[String]) =
    val maze = parseMaze(input)
    val adjacency = (coord: Coord) =>
      coord.adjacent.filter(maze.hasCoord).filter(neigh => maze(coord) - maze(neigh) < 2)
    pathFind(adjacency, maze.end, maze(_) == 'a').map(_.distance).get
