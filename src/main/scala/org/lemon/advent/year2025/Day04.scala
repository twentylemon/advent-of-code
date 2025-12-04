package org.lemon.advent.year2025

import org.lemon.advent.lib._
import org.lemon.advent.lib.`2d`._

private object Day04:

  def parse(input: String) =
    Coord.gridToMap(input).filter(_._2 == '@').keySet

  def removable(grid: Set[Coord]) =
    grid.filter(coord => coord.surrounding.count(grid) < 4)

  def part1(input: String) =
    val grid = parse(input)
    removable(grid).size

  def part2(input: String) =
    val grid = parse(input)
    Iterator.unfold(grid)(grid =>
      val toRemove = removable(grid)
      Option.when(toRemove.nonEmpty)((toRemove.size, grid -- toRemove))
    ).sum
