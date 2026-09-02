package org.lemon.advent.year2015

import org.lemon.advent.lib.*

private object Day09:

  def parse(input: String) = input.linesIterator.flatMap(_ match
    case s"$from to $to = $d" => Seq((from, to) -> d.toInt, (to, from) -> d.toInt)).toMap

  def part1(input: String) =
    val graph = parse(input)
    val cities = graph.keys.map(_._1).toSeq.distinct
    cities.permutations
      .map(_.sliding2.map(graph).sum)
      .min

  def part2(input: String) =
    val graph = parse(input)
    val cities = graph.keys.map(_._1).toSeq.distinct
    cities.permutations
      .map(_.sliding2.map(graph).sum)
      .max
