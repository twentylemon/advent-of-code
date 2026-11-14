package org.lemon.advent.year2023

import org.lemon.advent.lib.geometry.*
import org.lemon.advent.lib.graph.*

import scala.collection.immutable.BitSet
import scala.collection.mutable

private object Day23:

  def parse = Coord.gridToMap

  def startOf(grid: Map[Coord, Char]) = Area(grid).topRow.find(grid(_) == '.').get
  def endOf(grid: Map[Coord, Char]) = Area(grid).bottomRow.find(grid(_) == '.').get

  def neighbours(from: Coord, grid: Map[Coord, Char]) =
    grid(from) match
      case Direction(d) => Seq(from.move(d))
      case _ => from.adjacent

  def scenicRoute(start: Coord, end: Coord, edges: WeightedGraph[Coord, Int]) =
    val nodes = edges.keys.toIndexedSeq
    val index = nodes.zipWithIndex.toMap
    val neighbours = nodes.map(node => edges(node).map((neigh, dist) => (index(neigh), dist)).toIndexedSeq)
    val target = index(end)
    val paths = mutable.Buffer.empty[Int]
    val queue = mutable.Queue((BitSet(index(start)), index(start), 0))
    while queue.nonEmpty do
      val (path, cell, steps) = queue.dequeue()
      if cell == target then paths += steps
      else
        queue ++= neighbours(cell)
          .filterNot((neigh, _) => path(neigh))
          .map((neigh, dist) => (path + neigh, neigh, steps + dist))

    paths.max

  def trails(grid: Map[Coord, Char]) = grid
    .filter(_._2 != '#')
    .map((coord, _) => (coord, coord.adjacent.filter(c => grid.getOrElse(c, '#') != '#')))

  def landmarks(grid: Map[Coord, Char]) = junctions(trails(grid)) + startOf(grid) + endOf(grid)

  def part1(input: String) =
    val grid = parse(input)
    val possibleSteps =
      grid.map((coord, _) => (coord, neighbours(coord, grid).filter(c => grid.getOrElse(c, '#') != '#')))
    scenicRoute(startOf(grid), endOf(grid), compress(possibleSteps, landmarks(grid)))

  def part2(input: String) =
    val grid = parse(input)
    val possibleSteps = grid
      .filter(_._2 != '#')
      .map((coord, _) => (coord, coord.adjacent.filter(c => grid.getOrElse(c, '#') != '#')))
    scenicRoute(startOf(grid), endOf(grid), compress(possibleSteps, landmarks(grid)))
