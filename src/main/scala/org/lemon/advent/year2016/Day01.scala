package org.lemon.advent.year2016

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*

private object Day01:

  def parse(input: String) = input.linesIterator.flatMap(_.csv).map(x => (Direction(x.head), x.tail.toInt)).toSeq

  def walk(steps: Seq[(Direction, Int)]) =
    val turns = steps.scanLeft(Direction.Up) { case (facing, (turn, _)) =>
      turn match
        case Direction.Left => facing.turnLeft
        case Direction.Right => facing.turnRight
        case _ => throw AssertionError()
    }.tail

    turns.zip(steps)
      .flatMap { case (dir, (_, n)) => Seq.fill(n)(dir) }
      .scanLeft(Coord.origin)(_ + _)

  def part1(input: String) =
    val steps = parse(input)
    walk(steps).last.manhattan(Coord.origin)

  def part2(input: String) =
    val steps = parse(input)
    val positions = walk(steps)
    positions.scanLeft(Set.empty[Coord])(_ + _)
      .zip(positions)
      .collectFirst { case (seen, at) if seen(at) => at.manhattan(Coord.origin) }
      .get
