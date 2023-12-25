package org.lemon.advent.lib.graph

import scala.collection.mutable
import scala.math.Numeric.Implicits.infixNumericOps

case class Path[N, D](path: Seq[N], at: N, distance: D)

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the length of the shortest path between them.
  *
  * @param start the start node
  * @param end the end node
  * @param adjacency function to return edges for a given node
  * @param numeric contextual math ops for the distance
  * @return length of the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D](adjacency: N => Seq[(N, D)], start: N, end: N)(using numeric: Numeric[D]): Option[Path[N, D]] =
  given Ordering[Path[N, D]] = Ordering.by[Path[N, D], D](_.distance)
  val queue = mutable.PriorityQueue(Path(path = Seq(start), at = start, distance = numeric.zero))
  val visited = mutable.Set(start)

  while !queue.isEmpty && queue.head.at != end do
    val Path(path, at, distance) = queue.dequeue
    queue ++= adjacency(at)
      .filter((neigh, _) => visited.add(neigh))
      .map((node, dist) => Path(node +: path, node, distance + dist))

  queue.headOption

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the length of the shortest path between them. The distance between each node
  * is assumed to be one.
  *
  * @param start the start node
  * @param end the end node
  * @param adjacency function to return edges for a given node, all with distance one
  * @return length of the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  */
def pathFind[N](adjacency: N => Seq[N], start: N, end: N): Option[Path[N, Int]] =
  def unitAdjacency(node: N): Seq[(N, Int)] = adjacency(node).map((_, 1))
  pathFind(unitAdjacency, start, end)

/** Static adjacency list. A map of `node => [(neighbour, distance)...]`
  * @tparam N the node type
  * @tparam D the distance type
  */
type WeightedGraph[N, D] = Map[N, Seq[(N, D)]]

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the length of the shortest path between them.
  *
  * @param graph the graph to search in
  * @return length of the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D](graph: WeightedGraph[N, D], start: N, end: N)(using numeric: Numeric[D]): Option[Path[N, D]] =
  pathFind(graph.apply, start, end)

/** Static adjacency list. A map of `node => [neighbour...]`. The distances between all
  * nodes is assumed to be one.
  * @tparam N the node type
  */
type UnitGraph[N] = Map[N, Seq[N]]

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the length of the shortest path between them.
  *
  * @param start the start node
  * @param end the end node
  * @param graph the graph to search in
  * @return length of the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  */
def pathFind[N](graph: UnitGraph[N], start: N, end: N): Option[Path[N, Int]] =
  pathFind(graph.apply, start, end)
