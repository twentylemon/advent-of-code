package org.lemon.advent.year2025

import org.lemon.advent.lib.*
import org.lemon.advent.lib.graph.*

private object Day11:

  def parse(input: String) =
    import org.lemon.advent.lib.parse.*
    input.linesIterator.map(_ match
      case s"$from: ${Wsv(to*)}" => (from, to)
    ).toMap

  def countPaths(graph: Map[String, Seq[String]], start: String, end: String) =
    lazy val count: String => Long = memoize { (name: String) =>
      if name == end then 1L
      else graph.getOrElse(name, Seq.empty).map(count).sum
    }
    count(start)

  def part1(input: String) =
    val graph = parse(input)
    countPaths(graph, "you", "out")

  def part2(input: String) =
    val graph = parse(input)
    val dac = countPaths(graph, "svr", "dac") * countPaths(graph, "dac", "fft") * countPaths(graph, "fft", "out")
    val fft = countPaths(graph, "svr", "fft") * countPaths(graph, "fft", "dac") * countPaths(graph, "dac", "out")
    dac max fft
