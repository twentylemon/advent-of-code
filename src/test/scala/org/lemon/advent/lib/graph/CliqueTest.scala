package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.*
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*

class CliqueTest extends UnitTest:
  val undirectedGen: Gen[(Set[Int], Set[Set[Int]])] =
    for
      nodes <- Gen.resize(8, Gen.containerOf[Set, Int](arbitrary[Int]))
      edges <- Gen.someOf(nodes.subsets(2).toSeq)
    yield (nodes, edges.toSet)

  def maximalCliques(nodes: Set[Int], edges: Set[Set[Int]]): Set[Set[Int]] =
    val all = nodes.subsets().filter(_.subsets(2).forall(edges)).toSet
    all.filterNot(clique => all.exists(other => clique.size < other.size && clique.subsetOf(other)))

  def neighbours(edges: Set[Set[Int]])(node: Int): Set[Int] =
    edges.filter(_(node)).flatMap(_ - node)

  test("cliques finds every maximal clique") {
    check(forAllNoShrink(undirectedGen) { (nodes, edges) =>
      cliques(nodes, neighbours(edges)) == maximalCliques(nodes, edges)
    })
  }

  test("cliques ignores self loops") {
    check(forAllNoShrink(undirectedGen) { (nodes, edges) =>
      cliques(nodes, n => neighbours(edges)(n) + n) == maximalCliques(nodes, edges)
    })
  }

  test("cliques of a subset of nodes are the maximal cliques of its induced subgraph") {
    val gen =
      for
        (nodes, edges) <- undirectedGen
        subset <- Gen.someOf(nodes)
      yield (subset.toSet, edges)
    check(forAllNoShrink(gen) { (subset, edges) =>
      cliques(subset, neighbours(edges)) == maximalCliques(subset, edges)
    })
  }

  test("cliques of a unit graph finds every maximal clique") {
    check(forAllNoShrink(undirectedGen) { (nodes, edges) =>
      val graph = nodes.map(node => node -> neighbours(edges)(node)).toMap
      cliques(graph) == maximalCliques(nodes, edges)
    })
  }
