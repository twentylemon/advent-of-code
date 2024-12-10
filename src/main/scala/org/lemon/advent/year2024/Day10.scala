package org.lemon.advent.year2024

import scala.collection.mutable

import org.lemon.advent.lib.`2d`._
import org.lemon.advent.lib.graph._

private object Day10:

  def parse = Coord.gridToMap

  def part1(input: String) =
    val grid = parse(input)
    val adjacency = grid.map((c, h) => c -> c.adjacent.filter(grid.contains).filter(grid(_) == h + 1))
    grid
      .filter(_._2 == '0')
      .map((c, _) => fill(adjacency, c).count(grid(_) == '9'))
      .sum

  def enumerate(adjacency: Map[Coord, Seq[Coord]], start: Coord, ends: Set[Coord]) =
    val queue = mutable.Queue(start)
    var count = 0
    while queue.nonEmpty do
      val node = queue.dequeue
      if ends(node) then count += 1
      queue ++= adjacency(node)
    count

  def part2(input: String) =
    val grid = parse(input)
    val adjacency = grid.map((c, h) => c -> c.adjacent.filter(grid.contains).filter(grid(_) == h + 1))
    val ends = grid.filter(_._2 == '9').keySet
    grid
      .filter(_._2 == '0')
      .map((c, _) => enumerate(adjacency, c, ends))
      .sum
