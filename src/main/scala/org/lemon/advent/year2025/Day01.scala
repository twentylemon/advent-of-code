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
    (Seq(50) ++ turns.scanLeft(50)(_ + _)).sliding(2).map {
      case Seq(from, to) if from / 100 == to / 100 && from % 100 == 0 => 1
      case Seq(from, to) => ((from / 100).abs - (to / 100).abs).abs
    }.sum
