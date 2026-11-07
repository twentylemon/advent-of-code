package org.lemon.advent.year2024

import org.lemon.advent.lib.geometry.*
import org.lemon.advent.lib.graph.*

private object Day12:

  def parse = Coord.gridToMap

  def join(grid: Map[Coord, Char])(plots: DisjointSet[Coord], coord: Coord) =
    Seq(coord.right, coord.down)
      .filter(grid.get(_).contains(grid(coord)))
      .foldLeft(plots)(_.union(coord, _))

  def regions(grid: Map[Coord, Char]) =
    grid.keys.foldLeft(DisjointSet(grid.keys))(join(grid)).components

  def perimeter(region: Set[Coord]) = region.iterator.map(_.adjacent.count(!region(_))).sum

  def sides(region: Set[Coord]) =
    def in(coord: Coord) = region(coord)
    def out(coord: Coord) = !region(coord)
    def corners(coord: Coord) =
      // coord is adjacent to the outside, each contributes 1 side
      val topLeft = out(coord.up) && out(coord.left)
      val topRight = out(coord.up) && out(coord.right)
      val botLeft = out(coord.down) && out(coord.left)
      val botRight = out(coord.down) && out(coord.right)

      // coord is diagonal to the outside, the shape has turned a corner
      val diagUpLeft = in(coord.up) && in(coord.left) && out(coord.up.left)
      val diagUpRight = in(coord.up) && in(coord.right) && out(coord.up.right)
      val diagBotLeft = in(coord.down) && in(coord.left) && out(coord.down.left)
      val diagBotRight = in(coord.down) && in(coord.right) && out(coord.down.right)

      Seq(topLeft, topRight, botLeft, botRight, diagUpLeft, diagUpRight, diagBotLeft, diagBotRight)
        .count(identity)

    region.iterator.map(corners).sum

  def part1(input: String) =
    regions(parse(input))
      .map(region => region.size * perimeter(region))
      .sum

  def part2(input: String) =
    regions(parse(input))
      .map(region => region.size * sides(region))
      .sum
