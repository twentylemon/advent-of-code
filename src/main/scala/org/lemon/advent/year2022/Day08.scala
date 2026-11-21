package org.lemon.advent.year2022

import org.lemon.advent.lib.geometry.*

private object Day08:

  extension (seq: Seq[Int])
    def maxOrNegative: Int = if seq.isEmpty then -1 else seq.max

  def isVisible(heights: Seq[Seq[Int]], transposed: Seq[Seq[Int]], coord: Coord) =
    val height = heights(coord)
    heights(coord.y).slice(0, coord.x).maxOrNegative < height ||
    heights(coord.y).slice(coord.x + 1, heights(coord.y).size).maxOrNegative < height ||
    transposed(coord.x).slice(0, coord.y).maxOrNegative < height ||
    transposed(coord.x).slice(coord.y + 1, transposed(coord.x).size).maxOrNegative < height

  def parse(in: String) = in.linesIterator.map(_.toArray.map(_.asDigit).toSeq).toSeq

  def part1(input: String): Int =
    val heights = parse(input)
    val transposed = heights.transpose
    Area(heights).count(isVisible(heights, transposed, _))

  def countFrom(trees: List[Int], height: Int): Int = trees match
    case Nil => 0
    case head :: _ if head >= height => 1
    case head :: next if head < height => 1 + countFrom(next, height)
    case _ => throw AssertionError()

  def scenicScore(heights: Seq[Seq[Int]], transposed: Seq[Seq[Int]], coord: Coord) =
    val height = heights(coord)
    val left = heights(coord.y).slice(0, coord.x).reverse.toList
    val right = heights(coord.y).slice(coord.x + 1, heights(coord.y).size).toList
    val up = transposed(coord.x).slice(0, coord.y).reverse.toList
    val down = transposed(coord.x).slice(coord.y + 1, transposed(coord.x).size).toList
    countFrom(left, height) * countFrom(right, height) * countFrom(up, height) * countFrom(down, height)

  def part2(input: String): Int =
    val heights = parse(input)
    val transposed = heights.transpose
    Area(heights).contract(1).map(scenicScore(heights, transposed, _)).max
