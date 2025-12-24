package org.lemon.advent.year2016

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day08:

  sealed trait Instruction
  case class Rect(width: Int, height: Int) extends Instruction
  case class RotateRow(row: Int, shift: Int) extends Instruction
  case class RotateCol(col: Int, shift: Int) extends Instruction

  def parse(input: String) = input.linesIterator.map(_ match
    case s"rect ${a}x$b" => Rect(a.toInt, b.toInt)
    case s"rotate row y=$y by $s" => RotateRow(y.toInt, s.toInt)
    case s"rotate column x=$x by $s" => RotateCol(x.toInt, s.toInt)
  ).toSeq

  def run(input: String, area: Area) =
    parse(input).foldLeft(Set.empty[Coord])((state, instr) =>
      instr match
        case Rect(width, height) => state ++ Area(width, height)
        case RotateRow(row, shift) => state.map(c => if c.row == row then area.wrap(c.shiftRight(shift)) else c)
        case RotateCol(col, shift) => state.map(c => if c.col == col then area.wrap(c.shiftDown(shift)) else c)
    )

  def part1(input: String, width: Int = 50, height: Int = 6) =
    run(input, Area(width, height)).size

  def part2(input: String) =
    val area = Area(50, 6)
    val state = run(input, area)
    area.show(c => if state(c) then '#' else ' ')
