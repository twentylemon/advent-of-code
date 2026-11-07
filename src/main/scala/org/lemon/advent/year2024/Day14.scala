package org.lemon.advent.year2024

import org.lemon.advent.lib.*
import org.lemon.advent.lib.geometry.*

private object Day14:

  def parse(input: String) = input.linesIterator.map {
    case s"p=$px,$py v=$vx,$vy" => (Coord(px.toInt, py.toInt), Coord(vx.toInt, vy.toInt))
  }.toSeq

  def move(robits: Seq[(Coord, Coord)], area: Area): Seq[(Coord, Coord)] =
    robits.map((pos, vel) => (area.wrap(pos + vel), vel))

  def quads(robits: Seq[(Coord, Coord)], area: Area) =
    val (midX, midY) = (area.width / 2, area.height / 2)
    val counted = robits.map(_._1).filter(pos => pos.x != midX && pos.y != midY)
    area.quadrants.map(quad => counted.count(quad.contains))

  def part1(input: String, example: Boolean) =
    val area = if example then Area(11, 7) else Area(101, 103)
    val robits = parse(input)
    val later = Iterator.iterate(robits)(move(_, area)).nth(100)
    quads(later, area).product

  def part2(input: String) =
    val area = Area(101, 103)
    val robits = parse(input)
    val later = Iterator.iterate(robits)(move(_, area)).nth(8168)
    area.show(c => if later.exists(_._1 == c) then '#' else '.')
