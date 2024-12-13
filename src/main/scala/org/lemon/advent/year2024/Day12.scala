package org.lemon.advent.year2024

import scala.collection.mutable

import org.lemon.advent.lib.`2d`._
import org.lemon.advent.lib.graph._

private object Day12:

  def parse = Coord.gridToMap

  def adjacency(grid: Map[Coord, Char], start: Coord) =
    (c: Coord) => c.adjacent.filter(grid.contains).filter(grid(_) == grid(start))

  def region(grid: Map[Coord, Char], start: Coord) =
    Region(fill(adjacency(grid, start), start))

  def part1(input: String) =
    val grid = parse(input)
    val seen = mutable.Set.empty[Coord]
    grid.keys.iterator
      .filter(!seen.contains(_))
      .map(region(grid, _))
      .tapEach(region => seen.addAll(region.coords))
      .map(region => region.area * region.perimeterLength)
      .sum

  def part2(input: String) =
    val grid = parse(input)
    val seen = mutable.Set.empty[Coord]
    grid.keys.iterator
      .filter(!seen.contains(_))
      .map(region(grid, _))
      .tapEach(region => seen.addAll(region.coords))
      .map(region => region.area * region.sides)
      .sum
