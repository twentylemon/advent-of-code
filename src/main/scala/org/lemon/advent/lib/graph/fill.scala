package org.lemon.advent.lib.graph

import scala.collection.mutable

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
