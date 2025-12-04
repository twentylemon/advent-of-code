package org.lemon.advent.year2023

import org.lemon.advent.lib._

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

  def part2(input: String) =
    val grid = parse(input).map(_.reverse).transpose
    val cycle = findCycle(grid)(tiltCycle)
    cycle.stateAt(1_000_000_000)
      .map(load)
      .sum
