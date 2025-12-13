package org.lemon.advent.year2022

import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

private object Day12:

  def height(c: Char) =
    c match
      case 'S' => 'a'
      case 'E' => 'z'
      case c => c

  def part1(input: String) =
    val maze = Coord.gridToMap(input)
    val start = maze.find(_._2 == 'S').get._1
    val end = maze.find(_._2 == 'E').get._1
    val adjacency = (coord: Coord) =>
      coord.adjacent.filter(maze.contains).filter(neigh => height(maze(neigh)) - height(maze(coord)) < 2)
    pathFind(adjacency, start, end).map(_.distance).get

  def part2(input: String) =
    val maze = Coord.gridToMap(input)
    val end = maze.find(_._2 == 'E').get._1
    val adjacency = (coord: Coord) =>
      coord.adjacent.filter(maze.contains).filter(neigh => height(maze(coord)) - height(maze(neigh)) < 2)
    pathFind(adjacency, end, coord => height(maze(coord)) == 'a').map(_.distance).get
