package org.lemon.advent.year2025

import org.lemon.advent.lib._
import scala.collection.mutable

private object Day01:

  def parse(input: String) =
    import org.lemon.advent.lib.parse.{given, _}
    input.linesIterator.map(_ match
      case s"L$x" => -x.toInt
      case s"R$x" => x.toInt
    ).toSeq

  def part1(input: String) =
    val turns = parse(input)
    turns.scanLeft(50)(_ + _).count(_ % 100 == 0)

  def part2(input: String) =
    val turns = parse(input)
    var (pastZero, at) = (0, 50)
    turns.foreach(x =>
      val afterTurn = at + x
      pastZero += (afterTurn / 100).abs + (if at.signum == 0 || at.signum == afterTurn.signum then 0 else 1)
      at = afterTurn +% 100
    )
    pastZero
