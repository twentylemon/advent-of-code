package org.lemon.advent.year2023

private object Day13:

  def parse(input: String) = input.split("\n\n")
    .map(g => g.linesIterator.map(_.toCharArray.toSeq).toSeq)

  def reflecting(grid: Seq[Seq[Char]], diff: Int) =
    val i = grid.indices
      .drop(1)
      .map(y => (grid.take(y).reverse, grid.drop(y)))
      .map((top, bottom) => top.zip(bottom).map((l, r) => l.zip(r).count(_ != _)).sum)
      .indexWhere(_ == diff)
    if i >= 0 then i + 1 else -1

  def run(input: String, diff: Int) = parse(input)
    .map(grid =>
      val col = reflecting(grid, diff)
      if col >= 0 then (col, -1)
      else (-1, reflecting(grid.transpose, diff))
    )
    .map((c, r) => if r >= 0 then r else c*100)
    .sum

  def part1(input: String) = run(input, 0)

  def part2(input: String) = run(input, 1)
