package org.lemon.advent.year2023

import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.pairs

private object Day11:

  case class DarkEnergy(xs: Seq[Int], ys: Seq[Int])

  def parseGalaxies(input: String) = input.linesIterator
    .map(_.zipWithIndex.filter(_._1 == '#'))
    .zipWithIndex
    .flatMap((row, y) => row.map((_, x) => (x, y)))
    .toSeq

  def parseDarkEnergy(input: String) =
    def find(grid: Iterable[Iterable[Char]]) = grid
      .zipWithIndex
      .filter(_._1.forall('.'.equals))
      .map(_._2)
      .toSeq
    val lines = input.linesIterator.toSeq
    DarkEnergy(xs = find(lines.transpose), ys = find(lines.map(_.toCharArray)))

  def parse(input: String) = (parseGalaxies(input), parseDarkEnergy(input))

  def spaceDistance(lhs: Coord, rhs: Coord, energy: DarkEnergy, energyMult: Long): Long =
    val dist = lhs.manhattan(rhs)
    val (xRange, yRange) = (lhs.xRange(rhs), lhs.yRange(rhs))
    dist + (energyMult - 1) * (energy.xs.count(xRange.contains) + energy.ys.count(yRange.contains))

  def part1(input: String) =
    val (galaxies, energy) = parse(input)
    pairs(galaxies)
      .map(spaceDistance(_, _, energy, 2))
      .sum

  def part2(input: String, energyMult: Long) = 
    val (galaxies, energy) = parse(input)
    pairs(galaxies)
      .map(spaceDistance(_, _, energy, energyMult))
      .sum
