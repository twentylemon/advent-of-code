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
    def rel(x: Int) = if x < 0 then x / 100 - 1 else x / 100
    val s = turns.scanLeft(50)(_ + _).sliding(2).map {
      case Seq(from, to) if from % 100 == 0 =>
        (from / 100 - to / 100).abs - 1
      case Seq(from, to) if to % 100 == 0 =>
        (rel(from) - rel(to)).abs + 1
      case Seq(from, to) =>
        (rel(from) - rel(to)).abs
    }.sum
