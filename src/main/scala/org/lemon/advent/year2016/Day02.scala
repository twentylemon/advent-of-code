package org.lemon.advent.year2016

import org.lemon.advent.lib.`2d`.*

private object Day02:

  def parse(input: String) = input.linesIterator.map(_.map(Direction(_))).toSeq

  def part1(input: String) =
    val directions = parse(input)
    val area = Area(3, 3)
    directions.foldLeft((0, Coord(1, 1))) { case ((code, at), line) =>
      val end = line.foldLeft(at)((pos, dir) => area.clamp(pos + dir))
      (10 * code + 3 * end.y + end.x + 1, end)
    }._1

  def part2(input: String) =
    val directions = parse(input)
    val keypad = (Seq(Coord(2, 0) -> 1)
      ++ (2 to 4).map(n => Coord(n - 1, 1) -> n)
      ++ (5 to 9).map(n => Coord(n - 5, 2) -> n)
      ++ Seq(Coord(1, 3) -> 'A', Coord(2, 3) -> 'B', Coord(3, 3) -> 'C', Coord(2, 4) -> 'D')).toMap
    directions.foldLeft(("", Coord(0, 2))) { case ((code, at), line) =>
      val end = line.foldLeft(at)((pos, dir) =>
        val next = pos + dir
        if keypad.contains(next) then next else pos
      )
      (code + keypad(end).toString, end)
    }._1
