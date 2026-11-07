package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.scalacheck.*
import org.scalacheck.Prop.*

class DisjointSetTest extends UnitTest:
  def joined(graph: Graph) = graph.edges.foldLeft(DisjointSet(graph.nodes))((set, edge) => set.union(edge._1, edge._2))

  def reachable(graph: Graph, start: Int) =
    fill(graph.copy(edges = graph.edges ++ graph.reversed.edges).unitAdjacency, start)

  test("union connects its elements") {
    check(forAll(queryGen(maxSize = 50, maxEdgesPerNode = 2)) { case Query(graph, start, end) =>
      joined(graph).union(start, end).connected(start, end)
    })
  }

  test("union throws NoSuchElementException for a missing element") {
    a[NoSuchElementException] should be thrownBy DisjointSet(Seq(1, 2)).union(1, 3)
  }

  test("connected matches fill over the undirected graph") {
    check(forAll(queryGen(maxSize = 50, maxEdgesPerNode = 2)) { case Query(graph, start, end) =>
      joined(graph).connected(start, end) == reachable(graph, start).contains(end)
    })
  }

  test("connected throws NoSuchElementException for a missing element") {
    a[NoSuchElementException] should be thrownBy DisjointSet(Seq(1, 2)).connected(3, 1)
  }

  test("components match fill over the undirected graph") {
    check(forAll(graphGen(maxSize = 50, maxEdgesPerNode = 2)) { graph =>
      joined(graph).components.toSet == graph.nodes.map(reachable(graph, _)).toSet
    })
  }
