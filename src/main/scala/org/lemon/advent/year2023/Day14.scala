package org.lemon.advent.year2023

import scala.collection.mutable

private object Day14:

  def parse(input: String) = input.linesIterator
    .map(_.toCharArray.toSeq)
    .toSeq

  def tiltLeft(row: Seq[Char]) = Seq.unfold(row)(row =>
    if row.isEmpty then None
    else
      val (walls, tail) = row.span(_ == '#')
      val (space, tailtail) = tail.span(_ != '#')
      val rollyBoys = space.count(_ == 'O')
      Some((walls ++ Seq.fill(rollyBoys)('O') ++ Seq.fill(space.length - rollyBoys)('.'), tailtail))
  ).flatten

  def tiltGrid(grid: Seq[Seq[Char]]) = grid.map(tiltLeft)

  def load(row: Seq[Char]) = row.zipWithIndex
    .filter(_._1 == 'O')
    .map(row.size - _._2)
    .sum

  def part1(input: String) = 
    val grid = parse(input)
    tiltGrid(grid.transpose)
      .map(load)
      .sum
      
  def rotate(grid: Seq[Seq[Char]]) = grid.transpose.map(_.reverse)

  def tiltCycle =
    val rotation = tiltGrid andThen rotate
    rotation andThen rotation andThen rotation andThen rotation

  def cycle(grid: Seq[Seq[Char]], remaining: Int)(using memory: mutable.Map[Seq[Seq[Char]], Int]): Seq[Seq[Char]] =
    memory.get(grid) match
      case Some(cycleStartIndex) => 
        val cycleLength = memory.size - cycleStartIndex
        memory.find((_, at) => at == (cycleStartIndex + remaining % cycleLength)).get._1
      case None =>
        memory += (grid -> memory.size)
        cycle(tiltCycle(grid), remaining - 1)

  def part2(input: String) =
    val grid = parse(input).map(_.reverse).transpose
    given mutable.Map[Seq[Seq[Char]], Int] = mutable.Map.empty[Seq[Seq[Char]], Int]
    cycle(grid, 1_000_000_000)
      .map(load)
      .sum
