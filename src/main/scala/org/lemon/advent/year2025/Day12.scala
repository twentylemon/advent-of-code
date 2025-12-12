package org.lemon.advent.year2025

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day12:

  case class Problem(area: Area, requirements: Seq[Int])

  def parse(input: String) =
    import org.lemon.advent.lib.parse.*
    input match
      case Chunk(xs*) =>
        val shapes = xs
          .collect { case s"$x:\n$grid" => Coord.gridToMap(grid) }
        val trees = xs.last.linesIterator.map(_ match
          case s"${x}x$y: ${Wsv(ns*)}" =>
            Problem(Area(x.toInt, y.toInt), ns.map(_.toInt))
        ).toSeq
        (shapes, trees)

  def part1(input: String) =
    val (shapes, trees) = parse(input)
    trees.count(tree =>
      val minArea = tree.requirements.zip(shapes)
        .map((r, s) => r * s.values.count(_ == '#'))
        .sum
      tree.area.size >= minArea
    )
