package org.lemon.advent.year2024

import org.lemon.advent.lib.*
import org.lemon.advent.lib.geometry.*
import org.lemon.advent.lib.graph.*

private object Day15:

  def parseMoves(input: String): Seq[Direction] =
    input.filterNot(_.isWhitespace).map(Direction.apply)

  def parse(input: String): (Map[Coord, Char], Seq[Direction]) =
    val Seq(grid, moves) = input.chunks
    (Coord.gridToMap(grid), parseMoves(moves))

  def parseWide(input: String): (Map[Coord, Char], Seq[Direction]) =
    val Seq(grid, moves) = input.chunks
    val wide = grid.flatMap {
      case '#' => "##"
      case 'O' => "[]"
      case '.' => ".."
      case '@' => "@."
      case x => x.toString
    }
    (Coord.gridToMap(wide), parseMoves(moves))

  def step(grid: Map[Coord, Char], robit: Coord, dir: Direction): Map[Coord, Char] =
    def wouldBePushed(coord: Coord) =
      val next = coord + dir
      grid(next) match
        case 'O' => Seq(next)
        case '[' => Seq(next, next.right)
        case ']' => Seq(next.left, next)
        case _ => Seq()
    val moves = fill(wouldBePushed, robit)

    if moves.exists(c => grid(c + dir) == '#') then grid
    else
      val gapsFilled = moves.foldLeft(grid) { (g, coord) => g.updated(coord, '.') }
      moves.foldLeft(gapsFilled) { (g, coord) => g.updated(coord + dir, grid(coord)) }

  def gps(grid: Map[Coord, Char], box: Char) =
    grid.filter(_._2 == box).keysIterator.map(c => 100 * c.y + c.x).sum

  def part1(input: String) =
    val (grid, moves) = parse(input)
    val last = moves.foldLeft(grid)((g, d) => step(g, g.findValue('@').get, d))
    gps(last, 'O')

  def part2(input: String) =
    val (grid, moves) = parseWide(input)
    val last = moves.foldLeft(grid)((g, d) => step(g, g.findValue('@').get, d))
    gps(last, '[')
