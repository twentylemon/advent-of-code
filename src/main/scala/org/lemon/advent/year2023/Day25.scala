package org.lemon.advent.year2023

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

private object Day25:

  def parse(input: String) = input.linesIterator
    .map(_ match
      case s"$node: $edges" => edges.split(" ").map(_.trim).flatMap(e => Seq((node, e), (e, node)))
    )
    .flatten
    .toSeq
    .groupMap(_._1)(_._2)

  def sorted(edge: (String, String)) = if edge._1 < edge._2 then edge else edge.swap

  def totalPaths(graph: Map[String, Seq[String]], start: String, end: String) =
    type T = String
    def pathFind(usedEdges: Set[(T, T)]) =
      type Item = (Set[(T, T)], T, T, Int) // unusable-edges, from, to, distance
      given Ordering[Item] = Ordering.by[Item, Int](_._4).reverse
      val visited = mutable.Set.empty[T]
      val queue = mutable.PriorityQueue[Item]((usedEdges, null.asInstanceOf[T], start, 0))

      while !queue.isEmpty && queue.head._3 != end do
        val (unusable, from, at, distance) = queue.dequeue
        visited += at
        queue ++= graph(at)
          .filterNot(visited)
          .filter(next => !unusable(sorted((at, next))))
          .map(next => (unusable + sorted((at, next)), at, next, distance + 1))

      queue.headOption.map(item => item._1 + sorted((item._2, item._3)))

    @tailrec
    def count(usedEdges: Set[(T, T)], foundSoFar: Int): Int =
      if foundSoFar >= 4 then 4 // bail early
      else
        pathFind(usedEdges) match
          case None => foundSoFar
          case Some(edges) => count(usedEdges ++ edges, foundSoFar + 1)

    count(Set.empty[(T, T)], 0)

  def size[T](graph: Map[T, Seq[T]]): Int =
    val start = graph.head._1
    val nodes = mutable.Set(start)
    val queue = mutable.Queue(start)
    while !queue.isEmpty do
      val node = queue.dequeue
      queue ++= graph(node).filter(nodes.add)
    nodes.size

  def part1(input: String) =
    val graph = parse(input)
    val edges = graph.toSeq.flatMap((from, tos) => tos.map(to => sorted((from, to)))).toSet

    val threeCut = edges.par.filter((from, to) => totalPaths(graph, from, to) == 3)
    val cutGraph = threeCut.foldLeft(graph) { case (graph, (from, to)) =>
      graph.updated(from, graph(from).filter(_ != to)).updated(to, graph(to).filter(_ != from))
    }
    val cutSize = size(cutGraph)
    cutSize * (graph.size - cutSize)
