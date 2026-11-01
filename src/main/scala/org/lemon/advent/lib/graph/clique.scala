package org.lemon.advent.lib.graph

import scala.collection.mutable

/** Finds all maximal cliques in a graph. A maximal clique is a set of nodes where every
  * node is connected to every other node in the set, and the clique is not contained
  * within any other clique.
  *
  * @param nodes all the nodes in the graph
  * @param adjacency edges between nodes
  * @return set of all maximal cliques
  */
def cliques[N](nodes: Set[N], adjacency: N => Iterable[N]): Set[Set[N]] =
  val result = mutable.Set.empty[Set[N]]
  // a self loop would put a node in its own neighbourhood, so it would never be branched on
  def neighbours(node: N) = adjacency(node).toSet - node

  def bronKerbosch(r: Set[N], p: Set[N], x: Set[N]): Unit =
    if p.isEmpty && x.isEmpty then result.add(r)
    else
      var (xv, pv) = (x, p)
      val u = p.union(x).head
      for v <- p.diff(neighbours(u)) do
        val nv = neighbours(v)
        bronKerbosch(r + v, pv.intersect(nv), xv.intersect(nv))
        xv += v
        pv -= v

  bronKerbosch(Set.empty, nodes, Set.empty)
  result.toSet

/** Finds all maximal cliques in a graph. A maximal clique is a set of nodes where every
  * node is connected to every other node in the set, and the clique is not contained
  * within any other clique.
  *
  * @param graph the graph as an adjacency list
  * @return set of all maximal cliques
  */
def cliques[N](graph: UnitGraph[N]): Set[Set[N]] =
  cliques(graph.keySet, graph.withDefaultValue(Set.empty))
