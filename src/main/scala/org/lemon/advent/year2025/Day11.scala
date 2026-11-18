package org.lemon.advent.year2025

import org.lemon.advent.lib.*
import org.lemon.advent.lib.graph.*

private object Day11:

  def parse(input: String) =
    input.linesIterator.map {
      case s"$from: $rest" => (from, rest.wsv)
    }.toMap.withDefaultValue(Seq.empty)

  def part1(input: String) =
    val graph = parse(input)
    countPaths(graph, "you", "out")

  def part2(input: String) =
    val graph = parse(input)
    val dac = countPaths(graph, "svr", "dac") * countPaths(graph, "dac", "fft") * countPaths(graph, "fft", "out")
    val fft = countPaths(graph, "svr", "fft") * countPaths(graph, "fft", "dac") * countPaths(graph, "dac", "out")
    dac max fft
