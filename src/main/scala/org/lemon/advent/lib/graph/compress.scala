package org.lemon.advent.lib.graph

import scala.annotation.tailrec
import scala.math.Numeric.Implicits.*
import scala.math.Ordering.Implicits.*

/** Compresses a graph by keeping only the specified important nodes and computing
  * shortest path distances between them. Only nodes that are directly connected
  * via corridor paths (through non-important nodes) will have edges in the result.
  * Useful for graphs with many corridor-like paths that can be reduced to weighted
  * edges between junction nodes.
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
  def edgesFrom(start: N): Set[(N, D)] =
    @tailrec
    def search(queue: MinQueue[D, N], best: Map[N, D], found: Map[N, D]): Set[(N, D)] =
      queue.dequeue match
        case None => found.toSet
        case Some((cost, node, rest)) if cost > best(node) => search(rest, best, found)
        case Some((cost, node, rest)) if node != start && keepNodes.contains(node) =>
          search(rest, best, found.updated(node, cost))
        case Some((cost, node, rest)) =>
          val (nextQueue, nextBest) = adjacency(node).iterator
            .filter((next, _) => next != start)
            .foldLeft((rest, best)) { case (acc @ (queue, best), (next, dist)) =>
              val nextCost = cost + dist
              if best.get(next).exists(_ <= nextCost) then acc
              else (queue.enqueue(nextCost, next), best.updated(next, nextCost))
            }
          search(nextQueue, nextBest, found)

    val zero = Numeric[D].zero
    search(MinQueue(zero, start), Map(start -> zero), Map.empty)

  keepNodes.map(node => node -> edgesFrom(node)).toMap

/** Compresses a graph by keeping only the specified important nodes and computing
  * shortest path distances between them. The distance between each node is assumed to be one.
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
