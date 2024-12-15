package org.lemon.advent.year2024

import org.lemon.advent.lib._
import org.lemon.advent.lib.`2d`._

private object Day15:

  def parse(input: String): (Map[Coord, Char], Seq[Direction]) =
    val Array(grid, moves) = input.split("\n\n")
    (Coord.gridToMap(grid), moves.filterNot(_.isWhitespace).map(Direction.apply))

  def step(grid: Map[Coord, Char], robit: Coord, dir: Direction): Map[Coord, Char] =
    val moves = Iterator.iterate(robit)(_ + dir)
      .takeWhile(!grid.get(_).exists(c => c == '#' || c == '.'))
      .toSeq
    val dest = moves.last + dir
    if grid(dest) == '#' then grid
    else
      val shift = moves :+ dest
      shift.reverse.sliding(2).foldLeft(grid) {
        case (g, Seq(to, from)) => g.updated(to, g(from))
      }.updated(robit, '.')

  def part1(input: String) =
    val (grid, moves) = parse(input)
    val last = moves.foldLeft(grid)((g, d) => step(g, g.find(_._2 == '@').get._1, d))
    last
      .filter(_._2 == 'O')
      .keysIterator
      .map(c => 100 * c.y + c.x)
      .sum

  def part2(input: String) =
    0
