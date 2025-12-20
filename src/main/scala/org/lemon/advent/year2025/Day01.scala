package org.lemon.advent.year2025

import org.lemon.advent.lib.*

private object Day01:

  def parse(input: String) =
    input.linesIterator.map(_ match
      case s"L$x" => -x.toInt
      case s"R$x" => x.toInt
    ).toSeq

  def part1(input: String) =
    val turns = parse(input)
    turns.scanLeft(50)(_ + _).count(_ % 100 == 0)

  def part2(input: String) =
    val turns = parse(input)
    turns.scanLeft(50)(_ + _).sliding2.map {
      case (from, to) if from < to => Math.floorDiv(to, 100) - Math.floorDiv(from, 100)
      case (from, to) if from > to =>
        val minCentury = Math.floorDiv(to, 100) + (if to % 100 == 0 then 0 else 1)
        val maxCentury = Math.floorDiv(from - 1, 100)
        (maxCentury - minCentury + 1).max(0)
      case _ => 0
    }.sum
