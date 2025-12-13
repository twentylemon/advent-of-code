package org.lemon.advent.year2023

import org.lemon.advent.lib.*

private object Day13:

  def parse(input: String) = input.chunks
    .map(g => g.linesIterator.map(_.toCharArray.toSeq).toSeq)

  def reflecting(grid: Seq[Seq[Char]], diff: Int) =
    val idx = grid.indices
      .drop(1)
      .map(y => (grid.take(y).reverse, grid.drop(y)))
      .map((top, bottom) => top.zip(bottom).map((t, b) => t.zip(b).count(_ != _)).sum)
      .indexWhere(_ == diff)
    if idx >= 0 then idx + 1 else -1

  def run(input: String, diff: Int) = parse(input)
    .map(grid =>
      val col = reflecting(grid, diff)
      if col >= 0 then (col, -1)
      else (-1, reflecting(grid.transpose, diff))
    )
    .map((col, row) => if row >= 0 then row else col * 100)
    .sum

  def part1 = run(_, 0)

  def part2 = run(_, 1)
