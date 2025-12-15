package org.lemon.advent.year2023

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

private object Day21:

  def parse = Coord.gridToMap

  def count(grid: Map[Coord, Char], start: Coord)(depth: Int): Long =
    def adjacency(coord: Coord) = coord.adjacent.filter(c => grid.getOrElse(c, '#') != '#')
    distanceFrom(adjacency, start, depth).count((_, dist) => dist % 2 == depth % 2)

  def part1(input: String, depth: Int = 64) =
    val grid = parse(input)
    count(grid, grid.findValue('S').get)(depth)

  def square(n: Long): Long = n * n

  def part2(input: String, depth: Int = 26501365) =
    val grid = parse(input)
    val area = Area(grid)
    val size = area.width
    val start = grid.findValue('S').get

    assert(area.width == area.height, "grid is a square")
    assert(depth % area.width == area.width / 2, "start is in the middle")

    val numGridsOver = depth.toLong / size - 1
    val leftOverSteps = depth % size

    val numOddGrids = square((numGridsOver / 2) * 2 + 1)
    val numEvenGrids = square(((numGridsOver + 1) / 2) * 2)
    val odd = count(grid, start)(2 * size + 1)
    val even = count(grid, start)(2 * size)

    val left = count(grid, start.copy(x = area.left))(size - 1)
    val right = count(grid, start.copy(x = area.right))(size - 1)
    val top = count(grid, start.copy(y = area.top))(size - 1)
    val bottom = count(grid, start.copy(y = area.bottom))(size - 1)

    val smallTopLeft = count(grid, area.topLeft)(size / 2 - 1)
    val smallTopRight = count(grid, area.topRight)(size / 2 - 1)
    val smallBottomLeft = count(grid, area.bottomLeft)(size / 2 - 1)
    val smallBottomRight = count(grid, area.bottomRight)(size / 2 - 1)

    val bigTopLeft = count(grid, area.topLeft)(3 * size / 2 - 1)
    val bigTopRight = count(grid, area.topRight)(3 * size / 2 - 1)
    val bigBottomLeft = count(grid, area.bottomLeft)(3 * size / 2 - 1)
    val bigBottomRight = count(grid, area.bottomRight)(3 * size / 2 - 1)

    Seq[Long](
      odd * numOddGrids,
      even * numEvenGrids,
      left + right + top + bottom,
      (numGridsOver + 1) * (smallTopLeft + smallTopRight + smallBottomLeft + smallBottomRight),
      numGridsOver * (bigTopLeft + bigTopRight + bigBottomLeft + bigBottomRight),
    ).sum
