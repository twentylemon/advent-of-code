package org.lemon.advent.lib.graph

import scala.annotation.tailrec
import scala.collection.immutable.BitSet

/** Finds all maximal cliques in a graph. A maximal clique is a set of nodes where every
  * node is connected to every other node in the set, and the clique is not contained
  * within any other clique.
  *
  * @param nodes all the nodes in the graph
  * @param adjacency edges between nodes
  * @return set of all maximal cliques
  */
def cliques[N](nodes: Set[N], adjacency: N => Iterable[N]): Set[Set[N]] =
  val index = nodes.toIndexedSeq
  val id = index.zipWithIndex.toMap
  val neighbours =
    Array.tabulate(index.size)(i => BitSet.fromSpecific(adjacency(index(i)).iterator.flatMap(id.get)) - i)

  def bronKerbosch(r: List[Int], p: BitSet, x: BitSet, found: Set[Set[N]]): Set[Set[N]] =
    if p.isEmpty && x.isEmpty then found + r.iterator.map(index).toSet
    else
      @tailrec
      def each(vs: List[Int], p: BitSet, x: BitSet, found: Set[Set[N]]): Set[Set[N]] = vs match
        case Nil => found
        case v :: rest =>
          val nv = neighbours(v)
          each(rest, p - v, x + v, bronKerbosch(v :: r, p & nv, x & nv, found))

      val u = (p.iterator ++ x.iterator).maxBy(u => (p & neighbours(u)).size)
      each((p &~ neighbours(u)).toList, p, x, found)

  bronKerbosch(Nil, BitSet.fromSpecific(index.indices), BitSet.empty, Set.empty)

/** Finds all maximal cliques in a graph. A maximal clique is a set of nodes where every
  * node is connected to every other node in the set, and the clique is not contained
  * within any other clique.
  *
  * @param graph the graph as an adjacency list
  * @return set of all maximal cliques
  */
def cliques[N](graph: UnitGraph[N]): Set[Set[N]] =
  cliques(graph.keySet, graph)
