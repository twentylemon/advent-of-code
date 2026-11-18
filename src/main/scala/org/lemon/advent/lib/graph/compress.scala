package org.lemon.advent.lib.graph

import scala.collection.mutable
import scala.math.Numeric.Implicits.*
import scala.math.Ordering.Implicits.*

/** Compresses a graph by keeping only the specified important nodes and computing
  * shortest path distances between them. Only nodes that are directly connected
  * via corridor paths (through non-important nodes) will have edges in the result.
  * Each edge is the shortest such corridor, so parallel corridors between two nodes
  * collapse into one. Useful for graphs with many corridor-like paths that can be
  * reduced to weighted edges between junction nodes.
  *
  * @param adjacency function to return edges for a given node
  * @param keepNodes the important nodes to keep in the compressed graph
  * @return a weighted graph containing only the important nodes with edges weighted by shortest path distances
  * @tparam N the node type
  * @tparam D the distance type
  */
def compress[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    keepNodes: Set[N],
): WeightedGraph[N, D] =
  // one dijkstra per important node, which records but does not walk through the other important nodes
  def edgesFrom(start: N): Map[N, D] =
    val zero = Numeric[D].zero
    val best = mutable.HashMap(start -> zero)
    val queue = mutable.PriorityQueue((zero, start))(using Ordering.by[(D, N), D](_._1).reverse)
    val found = Map.newBuilder[N, D]
    while queue.nonEmpty do
      val (cost, node) = queue.dequeue()
      if cost <= best(node) then
        if node != start && keepNodes.contains(node) then found += node -> cost
        else
          for (next, dist) <- adjacency(node).iterator do
            val nextCost = cost + dist
            if best.get(next).forall(nextCost < _) then
              best(next) = nextCost
              queue.enqueue((nextCost, next))
    found.result()

  keepNodes.iterator.map(node => node -> edgesFrom(node)).toMap

/** Compresses a graph by keeping only the specified important nodes and computing
  * shortest path distances between them. The distance between each node is assumed to be one.
  * Only nodes connected by corridors through non-important nodes get an edge, weighted
  * by the shortest corridor, so parallel corridors between two nodes collapse into one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param keepNodes the important nodes to keep in the compressed graph
  * @return a weighted graph containing only the important nodes with edges weighted by shortest path distances
  * @tparam N the node type
  */
def compress[N](adjacency: N => Iterable[N] | Iterator[N], keepNodes: Set[N]): WeightedGraph[N, Int] =
  compress(unitAdjacency(adjacency), keepNodes)

/** Compresses a graph by keeping only the specified important nodes and computing
  * shortest path distances between them.
  * Only nodes connected by corridors through non-important nodes get an edge, weighted
  * by the shortest corridor, so parallel corridors between two nodes collapse into one.
  *
  * @param graph the graph to compress
  * @param keepNodes the important nodes to keep in the compressed graph
  * @return a weighted graph containing only the important nodes with edges weighted by shortest path distances
  * @tparam N the node type
  * @tparam D the distance type
  */
def compress[N, D: Numeric](graph: WeightedGraph[N, D], keepNodes: Set[N]): WeightedGraph[N, D] =
  compress(graph.apply, keepNodes)

/** Compresses a graph by keeping only the specified important nodes and computing
  * shortest path distances between them. The distance between each node is assumed to be one.
  * Only nodes connected by corridors through non-important nodes get an edge, weighted
  * by the shortest corridor, so parallel corridors between two nodes collapse into one.
  *
  * @param graph the graph to compress
  * @param keepNodes the important nodes to keep in the compressed graph
  * @return a weighted graph containing only the important nodes with edges weighted by shortest path distances
  * @tparam N the node type
  */
def compress[N](graph: UnitGraph[N], keepNodes: Set[N]): WeightedGraph[N, Int] =
  compress(graph.apply, keepNodes)

/** Finds junction nodes in the graph - nodes with more than two connections.
  * These are typically the important nodes to keep when compressing corridor-like graphs.
  *
  * @param graph the graph to find junctions in
  * @return the set of junction nodes
  * @tparam N the node type
  */
def junctions[N](graph: Map[N, Iterable[?]]): Set[N] =
  graph.filter(_._2.size > 2).keySet
