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
    grid.keys.foldLeft(DisjointSet(grid.keys))(join(grid)).components.map(Region(_))

  def part1(input: String) =
    regions(parse(input))
      .map(region => region.area * region.perimeterLength)
      .sum

  def part2(input: String) =
    regions(parse(input))
      .map(region => region.area * region.sides)
      .sum
