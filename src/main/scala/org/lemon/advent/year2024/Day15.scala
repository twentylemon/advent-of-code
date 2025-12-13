package org.lemon.advent.year2024

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day15:

  def parseMoves(input: String): Seq[Direction] =
    input.filterNot(_.isWhitespace).map(Direction.apply)

  def parse(input: String): (Map[Coord, Char], Seq[Direction]) =
    val Seq(grid, moves) = input.chunks
    (Coord.gridToMap(grid), parseMoves(moves))

  def parseWide(input: String): (Map[Coord, Char], Seq[Direction]) =
    val Seq(grid, moves) = input.chunks
    val wide = grid.flatMap(_ match
      case '#' => "##"
      case 'O' => "[]"
      case '.' => ".."
      case '@' => "@."
      case x => x.toString
    )
    (Coord.gridToMap(wide), parseMoves(moves))

  def step(grid: Map[Coord, Char], robit: Coord, dir: Direction): Map[Coord, Char] =
    val moves = Seq.unfold(Set(robit)) { moving =>
      if moving.isEmpty then None
      else
        val nexts = moving.map(_ + dir)
        val boxes = nexts.flatMap(p =>
          grid(p) match
            case 'O' => Seq(p)
            case '[' => Seq(p, p.right)
            case ']' => Seq(p.left, p)
            case _ => Seq()
        ).filterNot(moving)
        Some(moving -> boxes)
    }.flatten

    if moves.exists(c => grid(c + dir) == '#') then grid
    else
      val gapsFilled = moves.foldLeft(grid) { (g, coord) => g.updated(coord, '.') }
      moves.foldLeft(gapsFilled) { (g, coord) => g.updated(coord + dir, grid(coord)) }

  def gps(grid: Map[Coord, Char], box: Char) =
    grid.filter(_._2 == box).keysIterator.map(c => 100 * c.y + c.x).sum

  def part1(input: String) =
    val (grid, moves) = parse(input)
    val last = moves.foldLeft(grid)((g, d) => step(g, g.find(_._2 == '@').get._1, d))
    gps(last, 'O')

  def part2(input: String) =
    val (grid, moves) = parseWide(input)
    val last = moves.foldLeft(grid)((g, d) => step(g, g.find(_._2 == '@').get._1, d))
    gps(last, '[')
