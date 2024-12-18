package org.lemon.advent.year2024

import org.lemon.advent.lib._
import org.lemon.advent.lib.`2d`._
import org.lemon.advent.lib.graph._

private object Day18:

  def parse(input: String) = input.linesIterator.map(_ match
    case s"${Csv[Int](x, y)}" => Coord(x, y)
  ).toSeq

  def adjacency(area: Area, corrupt: Set[Coord])(coord: Coord): Seq[Coord] =
    coord.adjacent.filterNot(corrupt).filter(area.contains)

  def part1(input: String, example: Boolean = false, take: Int = 1024) =
    val area = if example then Area(0 to 6, 0 to 6) else Area(0 to 70, 0 to 70)
    val corrupt = parse(input).take(take).toSet
    pathFind(adjacency(area, corrupt), Coord(0, 0), area.bottomRight).get.distance

  def part2(input: String, example: Boolean = false) =
    val area = if example then Area(0 to 6, 0 to 6) else Area(0 to 70, 0 to 70)
    val bytes = parse(input)
    val lastPossible = bytes.inits.find(bits =>
      pathFind(adjacency(area, bits.toSet), Coord(0, 0), area.bottomRight).isDefined
    ).get
    val impossible = bytes(lastPossible.size)
    s"${impossible.x},${impossible.y}"
