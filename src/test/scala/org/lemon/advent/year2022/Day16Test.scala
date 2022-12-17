package org.lemon.advent.year2022

import org.lemon.advent._
import scala.collection.mutable
import scala.collection.BitSet

class Day16Test extends UnitTest {

  case class Valve(id: String, rate: Int, tunnels: Seq[String])

  def parseValve(in: String) = in match
    case s"Valve $id has flow rate=$rate; $_ $_ to $_ $tunnels" =>
      Valve(id = id, rate = rate.toInt, tunnels = tunnels.split(", "))

  def maxFlow(
      currentValve: String,
      time: Int,
      openFlowValves: BitSet,
      turn: Int
  )(using
      valves: Map[String, Valve],
      flowValves: Seq[Valve],
      memory: mutable.Map[String, Int] = mutable.Map()
  ): Int =
    val key = s"$turn#$currentValve#$time#${openFlowValves.hashCode}"

    memory.get(key) match
      case Some(memorized) => memorized
      case None =>
        val flow =
          if time <= 0 && turn > 0 then maxFlow("AA", 26, openFlowValves, turn - 1) // hard code restart for elebro. meh
          else if time <= 0 && turn == 0 then 0
          else
            val valve = valves(currentValve)
            val valveIndex = flowValves.indexOf(valve)
            val moveOn = valve.tunnels.map(v => maxFlow(v, time - 1, openFlowValves, turn))
            val open =
              if valveIndex < 0 || openFlowValves(valveIndex) then 0
              else (time - 1) * valve.rate + maxFlow(currentValve, time - 1, openFlowValves + valveIndex, turn)

            (moveOn ++ Seq(open)).max

        memory += (key -> flow)
        flow

  def part1(in: Seq[String]): Int =
    val allValves = in.map(parseValve)
    given Seq[Valve] = allValves.filter(_.rate > 0)
    given Map[String, Valve] = allValves.map(v => v.id -> v).toMap
    maxFlow("AA", 30, BitSet.empty, 0)

  def part2(in: Seq[String]): Int =
    val allValves = in.map(parseValve)
    given Seq[Valve] = allValves.filter(_.rate > 0)
    given Map[String, Valve] = allValves.map(v => v.id -> v).toMap
    maxFlow("AA", 26, BitSet.empty, 1)

  test("part 1 example") {
    val in = """|Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
                |Valve BB has flow rate=13; tunnels lead to valves CC, AA
                |Valve CC has flow rate=2; tunnels lead to valves DD, BB
                |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
                |Valve EE has flow rate=3; tunnels lead to valves FF, DD
                |Valve FF has flow rate=0; tunnels lead to valves EE, GG
                |Valve GG has flow rate=0; tunnels lead to valves FF, HH
                |Valve HH has flow rate=22; tunnel leads to valve GG
                |Valve II has flow rate=0; tunnels lead to valves AA, JJ
                |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin

    part1(in.linesIterator.toSeq) shouldBe 1651
  }

  test("part 1") {
    part1(readLines(file(2022)(16))) shouldBe 1737
  }

  test("part 2 example") {
    val in = """|Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
                |Valve BB has flow rate=13; tunnels lead to valves CC, AA
                |Valve CC has flow rate=2; tunnels lead to valves DD, BB
                |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
                |Valve EE has flow rate=3; tunnels lead to valves FF, DD
                |Valve FF has flow rate=0; tunnels lead to valves EE, GG
                |Valve GG has flow rate=0; tunnels lead to valves FF, HH
                |Valve HH has flow rate=22; tunnel leads to valve GG
                |Valve II has flow rate=0; tunnels lead to valves AA, JJ
                |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin

    part2(in.linesIterator.toSeq) shouldBe 1707
  }

  test("part 2") {
    part2(readLines(file(2022)(16))) shouldBe 2216
  }
}
