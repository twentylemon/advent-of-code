package org.lemon.advent.year2015

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day18:

  def parse(input: String) =
    val grid = Coord.gridToMap(input)
    (grid.filter(_._2 == '#').keySet, Area(grid))

  def tick(area: Area)(living: Set[Coord]) = living.view
    .flatMap(_.surrounding)
    .filter(area(_))
    .frequencies
    .collect {
      case (coord, 3) => coord
      case (coord, 2) if living(coord) => coord
    }.toSet

  def part1(input: String, steps: Int = 100) =
    val (initial, area) = parse(input)
    Iterator.iterate(initial)(tick(area)).nth(steps).size

  def part2(input: String, steps: Int = 100) =
    val (initial, area) = parse(input)
    val corners = area.corners.toSet
    Iterator.iterate(initial ++ corners)(tick(area)(_) ++ corners).nth(steps).size
