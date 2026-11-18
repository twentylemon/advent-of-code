package org.lemon.advent.year2023

import org.lemon.advent.lib.*
import org.lemon.advent.lib.graph.DisjointSet

import scala.annotation.tailrec
import scala.util.Random

private object Day25:

  extension (pair: (String, String))
    def sorted = if pair._1 < pair._2 then pair else pair.swap

  def parse(input: String) = input.linesIterator
    .map {
      case s"$node: $edges" => edges.wsv.flatMap(e => Seq((node, e), (e, node)))
    }
    .flatten
    .toSeq
    .groupMap(_._1)(_._2)

  def contract(nodes: Seq[String], edges: Seq[(String, String)]) =
    edges.iterator
      .scanLeft(DisjointSet(nodes)) { case (groups, (a, b)) => groups.union(a, b) }
      .find(_.components.size == 2)
      .get

  def part1(input: String) =
    val graph = parse(input)
    val edges = graph.toSeq.flatMap((from, tos) => tos.map(to => (from, to).sorted)).distinct

    @tailrec
    def cut: DisjointSet[String] =
      val groups = contract(graph.keys.toSeq, Random.shuffle(edges))
      if edges.count(!groups.connected(_, _)) == 3 then groups else cut

    cut.components.map(_.size).product
