package org.lemon.advent.lib.graph

import scala.collection.mutable
import scala.math.Numeric.Implicits.infixNumericOps

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
def allPaths[N](adjacency: N => Seq[N], start: N, ends: N => Boolean): Set[Path[N, Int]] =
  allPaths(unitAdjacency(adjacency), start, ends)
