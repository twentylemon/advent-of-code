package org.lemon.advent.year2024

import scala.collection.mutable

import org.lemon.advent.lib.`2d`._
import org.lemon.advent.lib.graph._

private object Day12:

  case class Plot(plot: Char, area: Set[Coord], perimeter: Set[Coord])

  def parse = Coord.gridToMap

  def adjacency(grid: Map[Coord, Char], start: Coord) =
    (c: Coord) => c.adjacent.filter(grid.contains).filter(grid(_) == grid(start))

  def price(grid: Map[Coord, Char], start: Coord) =
    val plot = grid(start)
    val area = fill(adjacency(grid, start), start)
    val perimeter = area.filter(_.adjacent.exists(!area.contains(_)))
    Plot(plot, area, perimeter)

  def perimeterLength(plot: Plot) =
    plot.perimeter.toSeq.map(_.adjacent.count(!plot.area.contains(_))).sum

  def part1(input: String) =
    val grid = parse(input)
    val seen = mutable.Set.empty[Coord]
    grid.keys.toSeq.view
      .filter(!seen.contains(_))
      .map(price(grid, _))
      .tapEach(p => seen.addAll(p.area))
      .map(plot => plot.area.size * perimeterLength(plot))
      .sum

  def part2(input: String) =
    0
