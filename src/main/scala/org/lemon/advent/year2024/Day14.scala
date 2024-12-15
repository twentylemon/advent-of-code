package org.lemon.advent.year2024

import org.lemon.advent.lib.`2d`._

private object Day14:

  def parse(input: String) = input.linesIterator.map(_ match
    case s"p=$px,$py v=$vx,$vy" => (Coord(px.toInt, py.toInt), Coord(vx.toInt, vy.toInt))
  ).toSeq

  def move(robits: Seq[(Coord, Coord)], area: Area): Seq[(Coord, Coord)] =
    robits.map((pos, vel) => ((pos + vel).shiftInto(area), vel))

  def quads(robits: Seq[(Coord, Coord)], area: Area) =
    val quadrants = area.quadrants
    quadrants.copy(
      topLeft = quadrants.topLeft.dropRight(1).dropBottom(1),
      topRight = quadrants.topRight.dropBottom(1),
      bottomLeft = quadrants.bottomLeft.dropRight(1),
    ).toSeq.map(quad => robits.count(r => quad.contains(r._1)))

  def part1(input: String, example: Boolean) =
    val area = if example then Area(11, 7) else Area(101, 103)
    val robits = parse(input)
    val later = Iterator.iterate(robits)(move(_, area)).drop(100).next
    quads(later, area).product

  def part2(input: String) =
    val area = Area(101, 103)
    val robits = parse(input)
    Iterator.iterate(robits)(move(_, area))
      .drop(8168)
      .map(robits => area.show(c => if robits.exists(_._1 == c) then '#' else '.'))
      .next
