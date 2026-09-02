package org.lemon.advent.lib.graph

import scala.collection.mutable

/** Performs a [topological sort](https://en.wikipedia.org/wiki/Topological_sorting) of the
  * directed graph using [Kahn's algorithm](https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm).
  * Returns an ordering of the given nodes such that for every edge `u -> v` returned by
  * `adjacency`, `u` appears before `v` in the result. Edges to nodes outside of `nodes`
  * are ignored, which makes it convenient to topologically sort a subset of a larger graph.
  *
  * If the graph (restricted to `nodes`) contains a cycle, no valid ordering exists and `None`
  * is returned. The ordering is otherwise deterministic and biased towards the iteration order
  * of `nodes`: when multiple nodes have no remaining prerequisites, the one that appeared
  * first in `nodes` is emitted first.
  *
  * @param nodes the nodes to include in the ordering
  * @param adjacency function returning the nodes that must come *after* the given node
  * @return an ordering of `nodes` consistent with `adjacency`, or `None` if a cycle exists
  * @tparam N the node type
  */
def topologicalSort[N](nodes: Iterable[N], adjacency: N => Iterable[N] | Iterator[N]): Option[Seq[N]] =
  val ordered = nodes.iterator.distinct.toVector
  val nodeSet = ordered.toSet
  val inDegree = mutable.LinkedHashMap.from(ordered.iterator.map(_ -> 0))
  for node <- ordered; neighbour <- adjacency(node) if nodeSet.contains(neighbour) do
    inDegree(neighbour) += 1
  val queue = mutable.Queue.from(ordered.filter(inDegree(_) == 0))
  val result = Vector.newBuilder[N]
  while queue.nonEmpty do
    val node = queue.dequeue()
    result += node
    for neighbour <- adjacency(node) if nodeSet.contains(neighbour) do
      inDegree(neighbour) -= 1
      if inDegree(neighbour) == 0 then queue.enqueue(neighbour)
  val sorted = result.result()
  Option.when(sorted.size == ordered.size)(sorted)

/** Performs a [topological sort](https://en.wikipedia.org/wiki/Topological_sorting) of the
  * directed graph using [Kahn's algorithm](https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm).
  * Returns an ordering of all the nodes in `graph` such that for every edge `u -> v`,
  * `u` appears before `v` in the result.
  *
  * If the graph contains a cycle, no valid ordering exists and `None` is returned.
  *
  * @param graph the graph as an adjacency list
  * @return an ordering of the nodes in `graph` consistent with the edges, or `None` if a cycle exists
  * @tparam N the node type
  */
def topologicalSort[N](graph: UnitGraph[N]): Option[Seq[N]] =
  topologicalSort(graph.keys, graph.withDefaultValue(Iterable.empty))
