package org.lemon.advent.year2023

import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.*

private object Day17:

  def parse(input: String) = input.linesIterator
    .map(_.toSeq.map(ch => ch.asDigit))
    .toSeq

  def solve(grid: Seq[Seq[Int]], minForward: Int, maxForward: Int) =
    val end = Area(grid).bottomRight
    case class State(at: Coord, direction: Direction, forward: Int)
    def adjacency(state: State) =
      val turns =
        if state.forward < minForward then Seq()
        else
          Seq(state.direction.turnLeft, state.direction.turnRight)
            .map(d => State(state.at + d, d, 1))
      val forward =
        if state.forward >= maxForward then Seq()
        else Seq(state.copy(at = state.at + state.direction, forward = state.forward + 1))

      (turns ++ forward)
        .filter(s => grid.hasCoord(s.at))
        .map(s => s -> grid(s.at))

    Seq(State(Coord.origin, Direction.Right, 0), State(Coord.origin, Direction.Down, 0))
      .flatMap(start => pathFind(adjacency, _.at.manhattan(end), start, _.at == end))
      .map(_.distance)
      .min

  def part1(input: String) = solve(parse(input), 1, 3)

  def part2(input: String) = solve(parse(input), 4, 10)
