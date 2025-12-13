package org.lemon.advent.year2022

private object Day08:

  extension (seq: Seq[Int])
    def maxOrNegative: Int = if seq.isEmpty then -1 else seq.max

  def isVisible(heights: Seq[Seq[Int]], transposed: Seq[Seq[Int]], x: Int, y: Int) =
    val height = heights(y)(x)
    heights(y).slice(0, x).maxOrNegative < height ||
    heights(y).slice(x + 1, heights(y).size).maxOrNegative < height ||
    transposed(x).slice(0, y).maxOrNegative < height ||
    transposed(x).slice(y + 1, transposed(x).size).maxOrNegative < height

  def parse(in: String) = in.linesIterator.map(_.toArray.map(_.asDigit).toSeq).toSeq

  def part1(input: String): Int =
    val heights = parse(input)
    val transposed = heights.transpose
    val visibles =
      for
        y <- heights.indices
        x <- heights(y).indices
      yield isVisible(heights, transposed, x, y)
    visibles.count(x => x)

  def countFrom(trees: List[Int], height: Int): Int = trees match
    case Nil => 0
    case head :: _ if head >= height => 1
    case head :: next if head < height => 1 + countFrom(next, height)
    case _ => throw AssertionError()

  def scenicScore(heights: Seq[Seq[Int]], transposed: Seq[Seq[Int]], x: Int, y: Int) =
    val height = heights(y)(x)
    val left = heights(y).slice(0, x).reverse.toList
    val right = heights(y).slice(x + 1, heights(y).size).toList
    val up = transposed(x).slice(0, y).reverse.toList
    val down = transposed(x).slice(y + 1, transposed(x).size).toList
    countFrom(left, height) * countFrom(right, height) * countFrom(up, height) * countFrom(down, height)

  def part2(input: String): Int =
    val heights = parse(input)
    val transposed = heights.transpose
    val scores =
      for
        y <- heights.indices.drop(1).dropRight(1)
        x <- heights(y).indices.drop(1).dropRight(1)
      yield scenicScore(heights, transposed, x, y)
    scores.max
