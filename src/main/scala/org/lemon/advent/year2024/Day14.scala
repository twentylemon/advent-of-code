package org.lemon.advent.year2024

import org.lemon.advent.lib.`2d`._

private object Day14:

  def parse(input: String) = input.linesIterator.map(_ match
    case s"p=$px,$py v=$vx,$vy" => (Coord(px.toInt, py.toInt), Coord(vx.toInt, vy.toInt))
  ).toSeq

  def move(robits: Seq[(Coord, Coord)], area: Area): Seq[(Coord, Coord)] =
    robits.map((pos, vel) => ((pos + vel).shiftInto(area), vel))

  def quads(robits: Seq[(Coord, Coord)], area: Area) =
    val size = (area.width / 2, area.height / 2)
    val upLeft = for x <- 0 until size._1; y <- 0 until size._2 yield Coord(x, y)
    val upRight = for x <- 0 until size._1; y <- 0 until size._2 yield Coord(area.width - x - 1, y)
    val downLeft = for x <- 0 until size._1; y <- 0 until size._2 yield Coord(x, area.height - y - 1)
    val downRight = for x <- 0 until size._1; y <- 0 until size._2 yield Coord(area.width - x - 1, area.height - y - 1)

    Seq(
      robits.filter(r => upLeft.contains(r._1)).size,
      robits.filter(r => upRight.contains(r._1)).size,
      robits.filter(r => downLeft.contains(r._1)).size,
      robits.filter(r => downRight.contains(r._1)).size,
    )

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
