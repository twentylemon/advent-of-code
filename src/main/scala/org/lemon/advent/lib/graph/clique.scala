package org.lemon.advent.lib.graph

import scala.annotation.tailrec

/** Finds all maximal cliques in a graph. A maximal clique is a set of nodes where every
  * node is connected to every other node in the set, and the clique is not contained
  * within any other clique.
  *
  * @param nodes all the nodes in the graph
  * @param adjacency edges between nodes
  * @return set of all maximal cliques
  */
def cliques[N](nodes: Set[N], adjacency: N => Iterable[N]): Set[Set[N]] =
  def neighbours(node: N) = adjacency(node).toSet - node

  def bronKerbosch(r: Set[N], p: Set[N], x: Set[N], found: Set[Set[N]]): Set[Set[N]] =
    if p.isEmpty && x.isEmpty then found + r
    else
      @tailrec
      def each(vs: List[N], p: Set[N], x: Set[N], found: Set[Set[N]]): Set[Set[N]] = vs match
        case Nil => found
        case v :: rest =>
          val nv = neighbours(v)
          each(rest, p - v, x + v, bronKerbosch(r + v, p.intersect(nv), x.intersect(nv), found))

      val u = p.union(x).head
      each(p.diff(neighbours(u)).toList, p, x, found)

  bronKerbosch(Set.empty, nodes, Set.empty, Set.empty)

/** Finds all maximal cliques in a graph. A maximal clique is a set of nodes where every
  * node is connected to every other node in the set, and the clique is not contained
  * within any other clique.
  *
  * @param graph the graph as an adjacency list
  * @return set of all maximal cliques
  */
def cliques[N](graph: UnitGraph[N]): Set[Set[N]] =
  cliques(graph.keySet, graph.withDefaultValue(Set.empty))
