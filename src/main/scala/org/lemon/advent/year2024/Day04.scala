package org.lemon.advent.year2024

import org.lemon.advent.lib.`2d`._

private object Day04:

  def phrase(wordsearch: Seq[String], coords: Seq[Coord]) = coords.map(wordsearch(_)).mkString

  def part1(input: String) =
    val lines = input.linesIterator.toSeq
    val area = Area(lines)

    (area.rows ++ area.cols ++ area.diagonals)
      .map(
        _.sliding(4)
          .map(phrase(lines, _))
          .count(s => s == "XMAS" || s == "XMAS".reverse)
      )
      .sum

  def part2(input: String) =
    val lines = input.linesIterator.toSeq
    val area = Area(lines)
    val mas = Set("MAS", "MAS".reverse)

    area.upDiagonals
      .map(
        _.sliding(3)
          .map(coords => (coords.head, phrase(lines, coords)))
          .filter((_, s) => mas.contains(s))
          .filter((start, _) => mas.contains(phrase(lines, area.downDiagonal(start.up.up).take(3).toSeq)))
          .size
      )
      .sum
