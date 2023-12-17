package org.lemon.advent.year2023

import org.lemon.advent.lib.`2d`.Coord._
import org.lemon.advent.lib.`2d`._

import scala.collection.mutable

private object Day17:

  def parse(input: String) = input.linesIterator
    .map(_.toSeq.map(ch => ch.asDigit))
    .toSeq

  def solve(grid: Seq[Seq[Int]], minForward: Int, maxForward: Int): Int =
    val end = Area(grid).bottomRight

    case class Step(at: Coord, direction: Direction, heatLoss: Int, forward: Int)
    val init = Step(at = origin, direction = Direction.Right, heatLoss = 0, forward = 0)
    val firstSteps = Seq(init, init.copy(direction = Direction.Down))

    given Ordering[Step] = Ordering[Int].on[Step](_.heatLoss).reverse
    val queue = mutable.PriorityQueue(firstSteps: _*)

    case class Seen(at: Coord, direction: Direction, forward: Int)
    val seen = mutable.Set(firstSteps.map(step => Seen(step.at, step.direction, step.forward)): _*)

    while !queue.isEmpty && queue.head.at != end do
      val step = queue.dequeue
      val loss = step.heatLoss + grid(step.at)

      val turns =
        if step.forward < minForward then Seq()
        else
          Seq(step.direction.turnLeft, step.direction.turnRight)
            .map(d => Step(step.at.move(d), d, loss, 1))
      val forward =
        if step.forward >= maxForward then Seq()
        else Seq(step.copy(at = step.at.move(step.direction), heatLoss = loss, forward = step.forward + 1))

      queue ++= (turns ++ forward)
        .filter(step => seen.add(Seen(step.at, step.direction, step.forward)))
        .filter(step => grid.hasCoord(step.at))

    queue.dequeue.heatLoss + grid(end) - grid(origin)

  def part1(input: String) = solve(parse(input), 1, 3)

  def part2(input: String) = solve(parse(input), 4, 10)
