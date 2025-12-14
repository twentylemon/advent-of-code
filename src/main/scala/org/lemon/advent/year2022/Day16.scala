package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import org.lemon.advent.lib.graph.*
import scala.collection.BitSet

private object Day16:

  case class Valve(id: String, rate: Int, tunnels: Seq[String])

  def parseValve(in: String) = in match
    case s"Valve $id has flow rate=$rate; $_ $_ to $_ $tunnels" =>
      Valve(id = id, rate = rate.toInt, tunnels = tunnels.csv)

  def maxFlow(startValve: String, timeLimit: Int, turnsRemaining: Int, allValves: Seq[Valve]): Int =
    val valveByName = allValves.map(v => v.id -> v).toMap
    val flowValves = allValves.filter(_.rate > 0)
    val flowValveIndex = flowValves.zipWithIndex.toMap

    val adjacency = (id: String) => valveByName(id).tunnels
    val sources = startValve +: flowValves.map(_.id)
    val distances = (for
      from <- sources
      to <- flowValves.map(_.id)
      path <- pathFind(adjacency, from, to)
    yield (from, to) -> path.distance).toMap

    lazy val compute: (String, Int, BitSet, Int) => Int = memoize { (valve, timeLeft, openValves, turnsLeft) =>
      val best = flowValveIndex
        .filterNot((_, idx) => openValves(idx))
        .flatMap((target, idx) =>
          distances.get((valve, target.id))
            .filter(d => d + 1 < timeLeft)
            .map(d =>
              val remaining = timeLeft - d - 1
              remaining * target.rate + compute(target.id, remaining, openValves + idx, turnsLeft)
            )
        )
        .maxOption
        .getOrElse(0)

      // elebro takes over
      if turnsLeft > 0 then best.max(compute(startValve, timeLimit, openValves, turnsLeft - 1))
      else best
    }

    compute(startValve, timeLimit, BitSet.empty, turnsRemaining)

  def part1(input: String): Int =
    val allValves = input.linesIterator.map(parseValve).toSeq
    maxFlow("AA", 30, 0, allValves)

  def part2(input: String): Int =
    val allValves = input.linesIterator.map(parseValve).toSeq
    maxFlow("AA", 26, 1, allValves)
