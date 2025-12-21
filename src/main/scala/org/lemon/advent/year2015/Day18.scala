package org.lemon.advent.year2015

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day18:

  def parse(input: String) =
    val grid = Coord.gridToMap(input)
    (grid.filter(_._2 == '#').keySet, Area(grid))

  def tick(area: Area)(living: Set[Coord]) = area.filter(coord =>
    val alive = coord.surrounding.count(living)
    if living(coord) then alive == 2 || alive == 3
    else alive == 3
  ).toSet

  def part1(input: String, steps: Int = 100) =
    val (initial, area) = parse(input)
    Iterator.iterate(initial)(tick(area)).nth(steps).size

  def part2(input: String, steps: Int = 100) =
    val (initial, area) = parse(input)
    val corners = Set(area.topLeft, area.topRight, area.bottomLeft, area.bottomRight)
    Iterator.iterate(initial ++ corners)(tick(area)(_) ++ corners).nth(steps).size
