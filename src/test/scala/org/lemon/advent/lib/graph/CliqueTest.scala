package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.*
import org.scalacheck.Prop.*

class CliqueTest extends UnitTest:
  val undirectedGen: Gen[(Int, Set[(Int, Int)])] =
    for
      size <- Gen.choose(1, 8)
      edges <- Gen.someOf(for a <- 0 until size; b <- a + 1 until size yield (a, b))
    yield (size, edges.toSet)

  def maximalCliques(size: Int, edges: Set[(Int, Int)]): Set[Set[Int]] =
    val all = (0 until size).toSet.subsets()
      .filter(_.nonEmpty)
      .filter(_.toSeq.pairs.forall((a, b) => edges((a min b, a max b))))
      .toSet
    all.filterNot(clique => all.exists(other => clique.size < other.size && clique.subsetOf(other)))

  def neighbours(edges: Set[(Int, Int)])(node: Int): Set[Int] =
    edges.collect { case (a, b) if a == node => b; case (a, b) if b == node => a }

  test("cliques finds every maximal clique") {
    check(forAll(undirectedGen) { (size, edges) =>
      size > 0 ==> (cliques((0 until size).toSet, neighbours(edges)) == maximalCliques(size, edges))
    })
  }

  test("cliques ignores self loops") {
    check(forAll(undirectedGen) { (size, edges) =>
      size > 0 ==> (cliques((0 until size).toSet, n => neighbours(edges)(n) + n) == maximalCliques(size, edges))
    })
  }
