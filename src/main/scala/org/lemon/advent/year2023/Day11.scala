package org.lemon.advent.year2023

import org.lemon.advent.Coord2._

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

  def pairs[T](xs: Iterable[T]) =
    for
      (x, ix) <- xs.zipWithIndex
      (y, iy) <- xs.zipWithIndex
      if ix < iy
    yield (x, y)

  def range(x: Int, y: Int) = math.min(x, y) to math.max(x, y)

  def spaceDistance(lhs: Coord, rhs: Coord, energy: DarkEnergy, energyMult: Long): Long =
    val dist = lhs.manhattan(rhs)
    val (xRange, yRange) = (range(lhs.x, rhs.x), range(lhs.y, rhs.y))
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
