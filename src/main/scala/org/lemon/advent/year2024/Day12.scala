package org.lemon.advent.year2024

import scala.collection.mutable

import org.lemon.advent.lib.`2d`._
import org.lemon.advent.lib.graph._

private object Day12:

  case class Plot(area: Set[Coord], perimeter: Set[Coord])

  def parse = Coord.gridToMap

  def adjacency(grid: Map[Coord, Char], start: Coord) =
    (c: Coord) => c.adjacent.filter(grid.contains).filter(grid(_) == grid(start))

  def plot(grid: Map[Coord, Char], start: Coord) =
    val area = fill(adjacency(grid, start), start)
    val perimeter = area.filter(_.surrounding.exists(!area.contains(_)))
    // perimeter includes "inner corners" which are diagonal to the outside
    Plot(area, perimeter)

  def perimeterLength(plot: Plot) =
    plot.perimeter.toSeq.map(_.adjacent.count(!plot.area.contains(_))).sum

  def part1(input: String) =
    val grid = parse(input)
    val seen = mutable.Set.empty[Coord]
    grid.keys.iterator
      .filter(!seen.contains(_))
      .map(plot(grid, _))
      .tapEach(p => seen.addAll(p.area))
      .map(plot => plot.area.size * perimeterLength(plot))
      .sum

  def numberOfSides(plot: Plot) =
    def in(coord: Coord) = plot.area(coord)
    def out(coord: Coord) = !plot.area(coord)
    def sides(coord: Coord) =
      // coord is adjacent to the outside, each contributes 1 side
      val topLeft = out(coord.up) && out(coord.left)
      val topRight = out(coord.up) && out(coord.right)
      val botLeft = out(coord.down) && out(coord.left)
      val botRight = out(coord.down) && out(coord.right)

      // coord is diagonal to the outside, the shape has turned a corner
      val holeUpLeft = in(coord.up) && in(coord.left) && out(coord.up.left)
      val holeUpRight = in(coord.up) && in(coord.right) && out(coord.up.right)
      val holeBotLeft = in(coord.down) && in(coord.left) && out(coord.down.left)
      val holeBotRight = in(coord.down) && in(coord.right) && out(coord.down.right)

      Seq(topLeft, topRight, botLeft, botRight, holeUpLeft, holeUpRight, holeBotLeft, holeBotRight)
        .count(identity)

    plot.perimeter.toSeq.map(sides).sum

  def part2(input: String) =
    val grid = parse(input)
    val seen = mutable.Set.empty[Coord]
    grid.keys.iterator
      .filter(!seen.contains(_))
      .map(plot(grid, _))
      .tapEach(p => seen.addAll(p.area))
      .map(plot => plot.area.size * numberOfSides(plot))
      .sum
