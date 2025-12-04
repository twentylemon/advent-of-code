package org.lemon.advent.year2023

import org.lemon.advent.lib.`2d`._
import org.lemon.advent.lib.graph._

import scala.collection.mutable

private object Day23:

  def parse = Coord.gridToMap

  def startOf(grid: Map[Coord, Char]) = Area(grid).topRow.find(grid(_) == '.').get
  def endOf(grid: Map[Coord, Char]) = Area(grid).bottomRow.find(grid(_) == '.').get

  def neighbours(from: Coord, grid: Map[Coord, Char]) =
    grid(from) match
      case '<' => Seq(from.left)
      case '>' => Seq(from.right)
      case '^' => Seq(from.up)
      case 'v' => Seq(from.down)
      case _ => from.adjacent

  def scenicRoute(start: Coord, end: Coord, edges: WeightedGraph[Coord, Int]) =
    val paths = mutable.Buffer.empty[Int]
    val queue = mutable.Queue((Set(start), start, 0))
    while !queue.isEmpty do
      val (path, cell, steps) = queue.dequeue
      if cell == end then paths += steps
      else
        queue ++= edges(cell)
          .filterNot((coord, _) => path.contains(coord))
          .map((coord, dist) => (path + coord, coord, steps + dist))

    paths.max

  def part1(input: String) =
    val grid = parse(input)
    val possibleSteps =
      grid.map((coord, _) => (coord, neighbours(coord, grid).filter(c => grid.getOrElse(c, '#') != '#')))
    scenicRoute(startOf(grid), endOf(grid), possibleSteps.mapValues(s => s.map((_, 1))).toMap)

  def part2(input: String) =
    val grid = parse(input)
    val possibleSteps = grid
      .filter(_._2 != '#')
      .map((coord, _) => (coord, coord.adjacent.filter(c => grid.getOrElse(c, '#') != '#')))
    val nodes = junctions(possibleSteps) + startOf(grid) + endOf(grid)
    scenicRoute(startOf(grid), endOf(grid), compress(possibleSteps, nodes))
