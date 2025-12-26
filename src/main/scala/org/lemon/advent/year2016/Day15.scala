package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day15:

  case class Disc(id: Int, positions: Int, starting: Int):
    def rotate(steps: Int) = copy(starting = (starting + steps) % positions)
    def posAt(time: Int) = (starting + time) % positions

  def parse(input: String) = input.linesIterator.map {
    case s"Disc #$id has $n positions; at time=0, it is at position $p." => Disc(id.toInt, n.toInt, p.toInt)
  }.toSeq

  def prize(discs: Seq[Disc]) =
    Iterator.unfold((0, 1, 0))((time, step, index) =>
      if index >= discs.length then None
      else
        val disc = discs(index)
        val nextTime = Iterator.from(0).map(i => time + i * step).find(t => disc.posAt(t + disc.id) == 0).get
        Some(nextTime -> (nextTime, step.lcm(disc.positions), index + 1))
    )

  def part1(input: String) =
    val discs = parse(input)
    prize(discs).last

  def part2(input: String) =
    val discs = parse(input)
    prize(discs :+ Disc(discs.size + 1, 11, 0)).last
