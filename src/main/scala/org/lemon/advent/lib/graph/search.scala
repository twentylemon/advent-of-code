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
  val queue = mutable.PriorityQueue(Path(path = Seq(start), distance = summon[Numeric[D]].zero))
  val visited = mutable.Set(start)

  while !queue.isEmpty && !ends(queue.head.at) do
    val node @ Path(path, distance) = queue.dequeue
    queue ++= adjacency(node.at)
      .filter((neigh, _) => visited.add(neigh))
      .map((neigh, dist) => Path(neigh +: path, distance + dist))

  queue.headOption

def allShortestPaths[N, D: Numeric](
    adjacency: N => Seq[(N, D)],
    start: N,
    ends: N => Boolean,
    best: D
): Set[Path[N, D]] =
  given Ordering[Path[N, D]] = Ordering.by[Path[N, D], D](_.distance).reverse
  val paths = mutable.Set.empty[Path[N, D]]
  val queue = mutable.PriorityQueue(Path(path = Seq(start), distance = summon[Numeric[D]].zero))

  var exit = false
  while !queue.isEmpty && !exit do
    val node @ Path(path, distance) = queue.dequeue
    if ends(node.at) then
      if best == distance then
        println(s"match: $distance  $node")
        paths.add(node)
    else
      queue ++= adjacency(node.at)
        .filter((_, d) => distance + d <= best)
        .map((neigh, dist) => Path(neigh +: path, distance + dist))

  paths.toSet

def allShortestPaths2[N, D: Numeric](adjacency: N => Seq[(N, D)], start: N, ends: N => Boolean): Set[Path[N, D]] =
  val visited = mutable.Set.empty[N]
  val paths = mutable.Set.empty[Path[N, D]]

  def dfs(loc: N, dist: D, path: Seq[N]): Unit =
    if !visited(loc) then
      if ends(loc) then
        println(s"found: $dist  $path")
        paths.add(Path(loc +: path, dist))
      else
        visited.add(loc)
        adjacency(loc).foreach((neigh, d) => dfs(neigh, dist + d, loc +: path))
        visited.remove(loc)
  dfs(start, summon[Numeric[D]].zero, Seq.empty)
  paths.toSet

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
