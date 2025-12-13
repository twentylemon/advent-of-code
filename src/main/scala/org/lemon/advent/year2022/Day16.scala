package org.lemon.advent.year2022

import org.lemon.advent.lib.*
import scala.collection.mutable
import scala.collection.BitSet

private object Day16:

  case class Valve(id: String, rate: Int, tunnels: Seq[String])

  def parseValve(in: String) = in match
    case s"Valve $id has flow rate=$rate; $_ $_ to $_ $tunnels" =>
      Valve(id = id, rate = rate.toInt, tunnels = tunnels.csv)

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
    val key = s"$turn#$currentValve#$time#${openFlowValves.toBitMask(0)}"

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

  def part1(input: Seq[String]): Int =
    val allValves = input.map(parseValve)
    given Seq[Valve] = allValves.filter(_.rate > 0)
    given Map[String, Valve] = allValves.map(v => v.id -> v).toMap
    maxFlow("AA", 30, BitSet.empty, 0)

  def part2(input: Seq[String]): Int =
    val allValves = input.map(parseValve)
    given Seq[Valve] = allValves.filter(_.rate > 0)
    given Map[String, Valve] = allValves.map(v => v.id -> v).toMap
    maxFlow("AA", 26, BitSet.empty, 1)
