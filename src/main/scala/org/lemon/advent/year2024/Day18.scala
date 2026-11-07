package org.lemon.advent.year2024

import org.lemon.advent.lib.*
import org.lemon.advent.lib.geometry.*
import org.lemon.advent.lib.graph.*

private object Day18:

  def parse(input: String) = input.linesIterator.map {
    case s"$x,$y" => Coord(x.toInt, y.toInt)
  }.toSeq

  def adjacency(area: Area, corrupt: Set[Coord])(coord: Coord): Seq[Coord] =
    coord.adjacent.filterNot(corrupt).filter(area.contains)

  def join(open: Set[Coord])(cells: DisjointSet[Coord], coord: Coord) =
    coord.adjacent.filter(open).foldLeft(cells)(_.union(coord, _))

  def part1(input: String, example: Boolean = false, take: Int = 1024) =
    val area = if example then Area(0 to 6, 0 to 6) else Area(0 to 70, 0 to 70)
    val corrupt = parse(input).take(take).toSet
    pathFind(adjacency(area, corrupt), Coord(0, 0), area.bottomRight).get.distance

  def part2(input: String, example: Boolean = false) =
    val area = if example then Area(0 to 6, 0 to 6) else Area(0 to 70, 0 to 70)
    val bytes = parse(input)
    val open = area.iterator.toSet -- bytes
    val cells = open.foldLeft(DisjointSet(area.iterator.toSeq))(join(open))

    @annotation.tailrec
    def loop(cells: DisjointSet[Coord], open: Set[Coord], fallen: List[Coord]): Coord =
      val freed = open + fallen.head
      val joined = join(freed)(cells, fallen.head)
      if joined.connected(Coord(0, 0), area.bottomRight) then fallen.head else loop(joined, freed, fallen.tail)

    val impossible = loop(cells, open, bytes.reverse.toList)
    s"${impossible.x},${impossible.y}"
