package org.lemon.advent.year2024

import org.lemon.advent.lib._
import org.lemon.advent.lib.`2d`._

private object Day15:

  def parseMoves(input: String): Seq[Direction] =
    input.filterNot(_.isWhitespace).map(Direction.apply)

  def parse(input: String): (Map[Coord, Char], Seq[Direction]) =
    val Array(grid, moves) = input.split("\n\n")
    (Coord.gridToMap(grid), parseMoves(moves))

  def parseWide(input: String): (Map[Coord, Char], Seq[Direction]) =
    val Array(grid, moves) = input.split("\n\n")
    val wide = grid.flatMap(_ match
      case '#' => "##"
      case 'O' => "[]"
      case '.' => ".."
      case '@' => "@."
      case x => x.toString
    )
    (Coord.gridToMap(wide), parseMoves(moves))

  def step(grid: Map[Coord, Char], robit: Coord, dir: Direction): Map[Coord, Char] =
    println(s"robit = $robit  moving = $dir")

    val moves = Iterator.unfold(Set(robit)) { moving =>
      println(s"moving = $moving")
      if moving.isEmpty then None
      else
        val nexts = moving.map(_ + dir)
        println(s"nexts = $nexts")
        val boxes = nexts.flatMap(p =>
          grid(p) match
            case 'O' => Seq(p)
            case '[' => Seq(p, p.right)
            case ']' => Seq(p.left, p)
            case _ => Seq()
        ).filterNot(moving)
        println(s"boxes = $boxes")
        Some(moving -> boxes)
    }.toSeq

    println(s"moves = $moves")
    val ret =
      if moves.isEmpty then grid
      else
        val dest = moves.last.map(_ + dir)
        if dest.exists(grid(_) == '#') then grid
        else
          val gapsFilled = moves.foldLeft(grid) { (g, moving) =>
            moving.foldLeft(g) { (g, coord) => g.updated(coord, '.') }
          }
          moves.foldLeft(gapsFilled) { (g, moving) =>
            moving.foldLeft(g) { (g, coord) => g.updated(coord + dir, grid(coord)) }
          }

    println(Area(ret).show(ret))
    ret

  def gps(grid: Map[Coord, Char], box: Char) =
    grid.filter(_._2 == box).keysIterator.map(c => 100 * c.y + c.x).sum

  def part1(input: String) =
    val (grid, moves) = parse(input)
    val last = moves.foldLeft(grid)((g, d) => step(g, g.find(_._2 == '@').get._1, d))
    gps(last, 'O')

  def part2(input: String) =
    val (grid, moves) = parseWide(input)
    println(Area(grid).show(grid))
    val last = moves.foldLeft(grid)((g, d) => step(g, g.find(_._2 == '@').get._1, d))
    gps(last, '[')
