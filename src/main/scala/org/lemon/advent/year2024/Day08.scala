package org.lemon.advent.year2024

import org.lemon.advent.lib._
import org.lemon.advent.lib.`2d`._

private object Day08:

  def parse = Coord.gridToMap

  def antinodes(grid: Map[Coord, Char])(lhs: Coord, rhs: Coord): Iterator[Coord] =
    def it(d: Coord) = Iterator.iterate(lhs)(_ + d.reduce).takeWhile(grid.contains)
    it(lhs - rhs) ++ it(rhs - lhs)

  def count(grid: Map[Coord, Char], cond: (Coord, Coord, Coord) => Boolean) =
    val freqs = grid.filterNot(_._2 == '.').groupBy(_._2).values.map(_.keys)
    freqs
      .flatMap(coords => coords.pairs.flatMap((a, b) => antinodes(grid)(a, b).filter(cond(a, b, _))))
      .toSet.size

  def part1(input: String) = count(
    parse(input),
    (lhs, rhs, c) =>
      val (l, r) = (lhs.manhattan(c), rhs.manhattan(c))
      l == 2 * r || r == 2 * l
  )

  def part2(input: String) = count(parse(input), (_, _, _) => true)
