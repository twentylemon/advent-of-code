package org.lemon.advent.lib.graph

import scala.collection.mutable
import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D: Numeric](adjacency: N => Seq[(N, D)], start: N, ends: N => Boolean): Option[Path[N, D]] =
  given Ordering[Path[N, D]] = Ordering.by[Path[N, D], D](_.distance).reverse
  val queue = mutable.PriorityQueue(Path(path = Vector(start), distance = summon[Numeric[D]].zero))
  val visited = mutable.Set(start)

  while !queue.isEmpty && !ends(queue.head.at) do
    val node @ Path(path, distance) = queue.dequeue
    queue ++= adjacency(node.at)
      .filter((neigh, _) => visited.add(neigh))
      .map((neigh, dist) => Path(path :+ neigh, distance + dist))

  queue.headOption

/** Finds all shortest paths in the graph from `start` to `end`.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return set of all shortest paths between `start` and `end`
  * @tparam N the node type
  * @tparam D the distance type
  */
def allShortestPaths[N, D: Numeric](adjacency: N => Seq[(N, D)], start: N, ends: N => Boolean): Set[Path[N, D]] =
  val paths = mutable.Set.empty[Path[N, D]]
  val queue = mutable.Queue(Path(path = Vector(start), distance = summon[Numeric[D]].zero))
  val costs = mutable.Map(start -> summon[Numeric[D]].zero)

  while !queue.isEmpty do
    val node @ Path(path, distance) = queue.dequeue
    if ends(node.at) then
      if paths.isEmpty || distance < paths.head.distance then paths.clear()
      if paths.isEmpty || distance <= paths.head.distance then paths.add(node)

    queue ++= adjacency(node.at)
      .filter((neigh, dist) =>
        val costTo = distance + dist
        costs.get(neigh) match
          case Some(known) if known < costTo => false
          case _ => costs(neigh) = costTo; true
      )
      .map((neigh, dist) => Path(path :+ neigh, distance + dist))

  paths.toSet

def allShortestPaths[N](adjacency: N => Seq[N], start: N, ends: N => Boolean): Set[Path[N, Int]] =
  allShortestPaths(unitAdjacency(adjacency), start, ends)

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
  pathFind(adjacency, start, end == _)

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  */
def pathFind[N](adjacency: N => Seq[N], start: N, ends: N => Boolean): Option[Path[N, Int]] =
  pathFind(unitAdjacency(adjacency), start, ends)

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
  pathFind(unitAdjacency(adjacency), start, end)

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
