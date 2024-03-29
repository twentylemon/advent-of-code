package org.lemon.advent.lib.graph

import scala.collection.mutable
import scala.math.Numeric.Implicits.infixNumericOps

case class Path[N, D](path: Seq[N], at: N, distance: D)

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D: Numeric](adjacency: N => Seq[(N, D)], start: N, end: N): Option[Path[N, D]] =
  given Ordering[Path[N, D]] = Ordering.by[Path[N, D], D](_.distance).reverse
  val queue = mutable.PriorityQueue(Path(path = Seq(start), at = start, distance = summon[Numeric[D]].zero))
  val visited = mutable.Set(start)

  while !queue.isEmpty && queue.head.at != end do
    val Path(path, at, distance) = queue.dequeue
    queue ++= adjacency(at)
      .filter((neigh, _) => visited.add(neigh))
      .map((node, dist) => Path(node +: path, node, distance + dist))

  queue.headOption

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  */
def pathFind[N](adjacency: N => Seq[N], start: N, end: N): Option[Path[N, Int]] =
  def unitAdjacency(node: N): Seq[(N, Int)] = adjacency(node).map((_, 1))
  pathFind(unitAdjacency, start, end)

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them.
  *
  * @param graph the graph to search in
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D: Numeric](graph: WeightedGraph[N, D], start: N, end: N): Option[Path[N, D]] =
  pathFind(graph.apply, start, end)

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them. The distance between each node is assumed to be one.
  *
  * @param graph the graph to search in
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  */
def pathFind[N](graph: UnitGraph[N], start: N, end: N): Option[Path[N, Int]] =
  pathFind(graph.apply, start, end)
