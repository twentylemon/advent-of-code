package org.lemon.advent.year2025

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day07:

  def parse(input: String) =
    val manifold = input.linesIterator.filter(line => line.contains('^') || line.contains('S'))
    Coord.gridToMap(manifold.mkString("\n"))

  def simulate(grid: Map[Coord, Char]) =
    val start = grid.find(_._2 == 'S').get._1

    def step(beams: Map[Coord, Long]) =
      beams.toSeq.flatMap((beam, count) =>
        grid(beam) match
          case '^' => Seq(beam.down.left -> count, beam.down.right -> count)
          case _ => Seq(beam.down -> count)
      ).groupMapReduce(_._1)(_._2)(_ + _)

    Iterator.iterate(Map(start -> 1L))(step)
      .takeWhile(beams => grid.contains(beams.keysIterator.next))
      .map(beams => beams.filter((c, _) => grid(c) == '^'))

  def part1(input: String) =
    val grid = parse(input)
    simulate(grid).map(_.size).sum

  def part2(input: String) =
    val grid = parse(input)
    simulate(grid).map(_.values.sum).sum + 1
