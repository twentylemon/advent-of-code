package org.lemon.advent.year2023

import org.lemon.advent.lib.graph.Path
import org.lemon.advent.lib.graph.pathFind

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

private object Day25:

  extension (pair: (String, String))
    def sorted = if pair._1 < pair._2 then pair else pair.swap

  def parse(input: String) = input.linesIterator
    .map(_ match
      case s"$node: $edges" => edges.split(" ").map(_.trim).flatMap(e => Seq((node, e), (e, node)))
    )
    .flatten
    .toSeq
    .groupMap(_._1)(_._2)

  def maxFlow[T](graph: Map[T, Seq[T]], edgeWeights: Map[(T, T), Int], source: T, sink: T) =
    Iterator.unfold(edgeWeights)(flowByEdge =>
      def adjacency(node: T) = graph(node).filter(next => flowByEdge((node, next)) > 0)
      pathFind(adjacency, source, sink) match
        case None => None
        case Some(Path(path, _, _)) =>
          val route = path.zip(path.tail).map(_.swap)
          val pathFlow = route.map(flowByEdge).min
          val nextFlows = route.foldLeft(flowByEdge) { case (flow, (from, to)) =>
            flow
              .updatedWith((from, to))(_.map(x => x - pathFlow))
              .updatedWith((to, from))(_.map(x => +pathFlow))
          }
          Some((pathFlow, nextFlows))
    ).sum

  def size[T](graph: Map[T, Seq[T]]) =
    val start = graph.head._1
    val nodes = mutable.Set(start)
    val queue = mutable.Queue(start)
    while !queue.isEmpty do
      val node = queue.dequeue
      queue ++= graph(node).filter(nodes.add)
    nodes.size

  def part1(input: String) =
    val graph = parse(input)
    val edges = graph.toSeq.flatMap((from, tos) => tos.map(to => (from, to).sorted)).toSet
    val flowByEdge =
      graph.toSeq.flatMap((from, tos) => tos.flatMap(to => Seq((from, to), (to, from)))).map((_, 1)).toMap

    val threeCut = edges.par.filter(maxFlow(graph, flowByEdge, _, _) == 3)
    val cutGraph = threeCut.foldLeft(graph) { case (graph, (from, to)) =>
      graph.updated(from, graph(from).filter(_ != to)).updated(to, graph(to).filter(_ != from))
    }
    val cutSize = size(cutGraph)
    cutSize * (graph.size - cutSize)
