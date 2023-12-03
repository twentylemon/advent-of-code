package org.lemon.advent.year2023

private object Day03:

  type Coord = (Int, Int)
  extension (coord: Coord)
    def row = coord._1
    def col = coord._2
    def up = (coord.row - 1, coord.col)
    def down = (coord.row + 1, coord.col)
    def left = (coord.row, coord.col - 1)
    def right = (coord.row, coord.col + 1)
    def surrounding = Seq(
      coord.up,
      coord.down,
      coord.left,
      coord.right,
      coord.up.left,
      coord.up.right,
      coord.down.left,
      coord.down.right
    )

  case class Part(num: Int, row: Int, bound: Range)
  case class Sym(sym: Char, coord: Coord)

  def parseLineParts(line: String, row: Int) =
    Iterator.unfold((line, 0))((tail, i) =>
      val (dots, numAndTail) = tail.span(!_.isDigit)
      val (num, tailtail) = numAndTail.span(_.isDigit)

      if num.isEmpty then None
      else
        val idx = i + dots.length
        val part = Part(num=num.toInt, row, bound=idx until (idx + num.length))
        Some((part, (tailtail, part.bound.end)))
    )

  def parseParts(input: String) = input.linesIterator
    .zipWithIndex
    .flatMap(parseLineParts)

  def isSymbol(char: Char) = !char.isLetterOrDigit && char != '.'

  def parseSymbols(input: String) = input.linesIterator
    .zipWithIndex
    .flatMap((line, row) =>
      line
        .zipWithIndex
        .filter((c, _) => isSymbol(c))
        .map((c, col) => Sym(sym=c, coord=(row, col)))
    )

  def findSurroundingParts(coord: Coord, parts: Iterable[Part]) =
    def collides(c: Coord, part: Part) = c.row == part.row && part.bound.contains(c.col)
    parts
      .filter(part => coord.surrounding.exists(collides(_, part)))

  def part1(input: String) =
    val parts = parseParts(input).toSeq
    parseSymbols(input)
      .map(_.coord)
      .flatMap(findSurroundingParts(_, parts))
      .distinct
      .map(_.num)
      .sum

  def part2(input: String) =
    val parts = parseParts(input).toSeq
    parseSymbols(input)
      .filter(_.sym == '*')
      .map(sym => findSurroundingParts(sym.coord, parts))
      .filter(_.size == 2)
      .map(_.map(part => part.num).product)
      .sum
  