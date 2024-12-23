package org.lemon.advent.year2024

import org.lemon.advent.lib._

import scala.collection.mutable

private object Day23:

  def parse(input: String) =
    import org.lemon.advent.lib.parse.{given, _}
    input.linesIterator.flatMap(_ match
      case s"$a-$b" => Seq((a, b), (b, a))
    )
      .toSeq
      .groupMap(_._1)(_._2)
      .mapValues(_.toSet)
      .toMap
      .withDefaultValue(Set.empty)

  def bronKerbosch(graph: Map[String, Set[String]])(
      r: Set[String],
      p: Set[String],
      x: Set[String]
  )(result: mutable.Set[Set[String]]): Unit =
    if p.isEmpty && x.isEmpty then result.add(r)
    else
      var (xv, pv) = (x, p)
      val u = p.union(x).head
      for v <- p.diff(graph(u)) do
        bronKerbosch(graph)(r + v, pv.intersect(graph(v)), xv.intersect(graph(v)))(result)
        xv += v
        pv -= v

  def part1(input: String) =
    val graph = parse(input)
    graph.keys
      .triples
      .filter((a, b, c) => a.startsWith("t") || b.startsWith("t") || c.startsWith("t"))
      .count((a, b, c) =>
        graph(a).contains(b) &&
          graph(b).contains(c) &&
          graph(c).contains(a)
      )

  def part2(input: String) =
    val graph = parse(input)
    val result = mutable.Set.empty[Set[String]]
    bronKerbosch(graph)(Set.empty, graph.keys.toSet, Set.empty)(result)
    val largest = result.maxBy(_.size)
    largest.toSeq.sorted.mkString(",")
