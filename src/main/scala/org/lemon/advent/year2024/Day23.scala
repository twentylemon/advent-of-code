package org.lemon.advent.year2024

import org.lemon.advent.lib._
import org.lemon.advent.lib.graph._

private object Day23:

  def parse(input: String) =
    import org.lemon.advent.lib.parse.{given, _}
    input.linesIterator.flatMap(_ match
      case s"$a-$b" => Seq((a, b), (b, a))
    )
      .toSeq
      .groupMapReduce(_._1)(x => Set(x._2))(_ ++ _)

  def part1(input: String) =
    val graph = parse(input)
    cliques(graph)
      .flatMap(_.triples)
      .map((a, b, c) => Seq(a, b, c).sorted)
      .count(_.exists(_.startsWith("t")))

  def part2(input: String) =
    val graph = parse(input)
    val largest = cliques(graph).maxBy(_.size)
    largest.toSeq.sorted.mkString(",")
