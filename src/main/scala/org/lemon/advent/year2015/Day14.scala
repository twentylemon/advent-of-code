package org.lemon.advent.year2015

import scala.math.Integral.Implicits.*

private object Day14:

  case class Reindeer(name: String, speed: Int, time: Int, rest: Int)

  def parse(input: String) = input.linesIterator.map(_ match
    case s"$r can fly $v km/s for $t seconds, but then must rest for $n seconds." =>
      Reindeer(name = r, speed = v.toInt, time = t.toInt, rest = n.toInt)
  ).toSeq

  def distanceAt(reindeer: Reindeer, at: Int) =
    val cycleTime = reindeer.time + reindeer.rest
    val (numCycles, remaining) = at /% cycleTime
    numCycles * reindeer.speed * reindeer.time + reindeer.time.min(remaining) * reindeer.speed

  def part1(input: String, at: Int = 2503) =
    val deer = parse(input)
    deer.map(distanceAt(_, at)).max

  def part2(input: String, at: Int = 2503) =
    val deer = parse(input)
    val positions = (1 to at).map(at => deer.map(distanceAt(_, at)))
    deer.indices.map(i => positions.count(ats => ats(i) == ats.max)).max
