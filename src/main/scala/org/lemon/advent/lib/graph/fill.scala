package org.lemon.advent.lib.graph

import scala.collection.mutable
import scala.math.Numeric.Implicits._
import scala.math.Ordering.Implicits._

/** Performs a breadth first fill of the graph from the starting node, returning
  * the set of all reachable nodes.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @return the set of reachable nodes from `start`
  * @tparam N the node type
  */
def fill[N](adjacency: N => Seq[N], start: N): Set[N] =
  val nodes = mutable.Set(start)
  val queue = mutable.Queue(start)
  while !queue.isEmpty do
    val node = queue.dequeue
    queue ++= adjacency(node).filter(nodes.add)
  nodes.toSet

/** Performs a breadth first fill of the graph from the starting node, returning
  * the distance to all reachable nodes.
  *
  * @param adjacency function to return edges for a given node
  * @param end the node to calculate distances to
  * @return the set of reachable nodes from `start`
  * @tparam N the node type
  * @tparam D the distance type
  */
def distanceFrom[N, D: Numeric](adjacency: N => Seq[(N, D)], end: N): Map[N, D] =
  val distances = mutable.Map(end -> summon[Numeric[D]].zero)
  given Ordering[(N, D)] = Ordering.by[(N, D), D](_._2)
  val queue = mutable.PriorityQueue(distances.head)

  while !queue.isEmpty do
    val node = queue.dequeue._1
    val dist = distances(node)
    queue ++= adjacency(node)
      .filter((neigh, d) => distances.get(neigh).forall(_ > dist + d))
      .tapEach((neigh, d) => distances(neigh) = dist + d)

  distances.toMap

/** Performs a breadth first fill of the graph from the starting node, returning
  * the distance to all reachable nodes. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node
  * @param end the node to calculate distances to
  * @return the set of reachable nodes from `start`
  * @tparam N the node type
  */
def distanceFrom[N](adjacency: N => Seq[N], end: N): Map[N, Int] =
  distanceFrom(unitAdjacency(adjacency), end)

/** Performs a breadth first fill of the graph from the starting node to the ending nodes, returning
  * all possible paths between the two sets.
  *
  * Note that if `adjacency` can form loops, this function will not terminate.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the set of all paths from `start` to any of `ends`
  * @tparam N the node type
  */
def allPaths[N, D: Numeric](adjacency: N => Seq[(N, D)], start: N, ends: N => Boolean): Set[Path[N, D]] =
  val paths = mutable.Set.empty[Path[N, D]]
  val queue = mutable.Queue(Path(path = Seq(start), distance = summon[Numeric[D]].zero))

  while queue.nonEmpty do
    val node @ Path(path, distance) = queue.dequeue
    if ends(node.at) then paths.add(node)
    queue ++= adjacency(node.at).map((neigh, dist) => Path(neigh +: path, distance + dist))
  paths.toSet

/** Performs a breadth first fill of the graph from the starting node to the ending nodes, returning
  * all possible paths between the two sets. The distance between each node is assumed to be one.
  *
  * Note that if `adjacency` can form loops, this function will not terminate.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the set of all paths from `start` to any of `ends`
  * @tparam N the node type
  */
def allPaths[N](adjacency: N => Seq[N], start: N, ends: N => Boolean): Set[Path[N, Int]] =
  allPaths(unitAdjacency(adjacency), start, ends)
