package org.lemon.advent.year2024

import org.lemon.advent.lib._
import org.lemon.advent.lib.`2d`._

private object Day08:

  def parse = Coord.gridToMap

  def antinodes(grid: Map[Coord, Char], cond: (Coord, Coord, Coord) => Boolean)(lhs: Coord, rhs: Coord): Set[Coord] =
    def it(d: Coord): Iterator[Coord] =
      Iterator.iterate(lhs)(_ + d)
        .takeWhile(grid.contains)
        .filter(c => cond(lhs, rhs, c))
    (it(lhs - rhs) ++ it(rhs - lhs)).toSet

  def count(grid: Map[Coord, Char], cond: (Coord, Coord, Coord) => Boolean) =
    val freqs = grid.view.filterNot(_._2 == '.').groupBy(_._2).mapValues(_.map(_._1).toSeq).values
    freqs
      .flatMap(coords => coords.pairs.flatMap(antinodes(grid, cond).tupled))
      .toSet.size

  def part1(input: String) = count(
    parse(input),
    (lhs, rhs, c) =>
      val (l, r) = (lhs.manhattan(c), rhs.manhattan(c))
      l == 2 * r || r == 2 * l
  )

  def part2(input: String) = count(parse(input), (_, _, _) => true)
