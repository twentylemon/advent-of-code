package org.lemon.advent.lib.graph

import org.lemon.advent.lib.*
import org.scalacheck.*
import org.scalacheck.Shrink.*

/** A small directed graph with positive weights, which may have parallel edges and self loops.
  * Searches are checked against brute force answers computed from the edge list.
  */
case class Graph(size: Int, edges: List[(Int, Int, Int)]):
  def nodes = 0 until size
  def adjacency(node: Int): List[(Int, Int)] = edges.collect { case (from, to, w) if from == node => (to, w) }
  def unitAdjacency(node: Int): List[Int] = adjacency(node).map(_._1).distinct
  def reversed = Graph(size, edges.map((from, to, w) => (to, from, w)))

  /** bellman-ford */
  def distancesFrom(start: Int): Map[Int, Int] =
    Iterator.iterate(Map(start -> 0))(dist =>
      edges.foldLeft(dist) { case (d, (from, to, w)) =>
        d.get(from) match
          case Some(df) if d.get(to).forall(_ > df + w) => d.updated(to, df + w)
          case _ => d
      }
    ).nth(size)

  /** every simple path from `start` to `end`, using the cheapest of any parallel edges */
  def simplePaths(start: Int, end: Int): Seq[Path[Int, Int]] =
    def cheapest(from: Int, to: Int) = adjacency(from).filter(_._1 == to).map(_._2).min
    def loop(path: Vector[Int], distance: Int): Seq[Path[Int, Int]] =
      if path.last == end then Seq(Path(path, distance))
      else
        unitAdjacency(path.last)
          .filterNot(path.contains)
          .flatMap(next => loop(path :+ next, distance + cheapest(path.last, next)))
    loop(Vector(start), 0)

case class Query(graph: Graph, start: Int, end: Int)

given Shrink[Graph] = Shrink { case graph: Graph =>
  shrink(graph.edges)
    .filter(_.forall((from, to, w) => graph.nodes.contains(from) && graph.nodes.contains(to) && w > 0))
    .map(edges => graph.copy(edges = edges))
}
given Shrink[Query] = Shrink { case Query(graph, start, end) => shrink(graph).map(Query(_, start, end)) }

def graphGen(maxSize: Int, maxEdgesPerNode: Int, maxEdgeWeight: Int = 20): Gen[Graph] =
  for
    size <- Gen.choose(1, maxSize)
    count <- Gen.choose(0, size * maxEdgesPerNode)
    edges <- Gen.listOfN(count, Gen.zip(Gen.choose(0, size - 1), Gen.choose(0, size - 1), Gen.choose(1, maxEdgeWeight)))
  yield Graph(size, edges)

def queryGen(maxSize: Int = 8, maxEdgesPerNode: Int = 4, maxEdgeWeight: Int = 20): Gen[Query] =
  for
    graph <- graphGen(maxSize, maxEdgesPerNode, maxEdgeWeight)
    start <- Gen.choose(0, graph.size - 1)
    end <- Gen.choose(0, graph.size - 1)
  yield Query(graph, start, end)
