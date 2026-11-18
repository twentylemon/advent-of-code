package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.*
import org.scalacheck.Prop.*

class CompressTest extends UnitTest:
  val gen =
    for
      graph <- graphGen(maxSize = 8, maxEdgesPerNode = 3)
      important <- Gen.someOf(graph.nodes)
    yield (graph, important.toSet)

  def corridorDistances(graph: Graph, important: Set[Int]): Map[Int, Set[(Int, Long)]] =
    important.map { from =>
      // can leave `from` and pass through unimportant nodes, but stops at the first other important node
      val corridors = graph.copy(edges = graph.edges.filter((f, t, _) => (f == from || !important(f)) && t != from))
      val distances = corridors.distancesFrom(from)
      from -> (important - from).flatMap(to => distances.get(to).map(to -> _))
    }.toMap

  def longDistances(compressed: WeightedGraph[Int, Int]): Map[Int, Set[(Int, Long)]] =
    compressed.view.mapValues(_.map((to, d) => (to, d.toLong)).toSet).toMap

  test("compress keeps the shortest corridor distance between each pair of important nodes") {
    check(forAll(gen) { (graph: Graph, important: Set[Int]) =>
      longDistances(compress(graph.adjacency, important)) == corridorDistances(graph, important)
    })
  }

  test("compress of a unit graph counts the steps between each pair of important nodes") {
    check(forAll(gen) { (graph: Graph, important: Set[Int]) =>
      val unitGraph = graph.nodes.map(node => node -> graph.unitAdjacency(node)).toMap
      val steps = graph.copy(edges = graph.edges.map((from, to, _) => (from, to, 1)))
      longDistances(compress(unitGraph, important)) == corridorDistances(steps, important)
    })
  }

  test("junctions are the nodes with more than two neighbours") {
    check((graph: Map[Int, List[Int]]) =>
      val found = junctions(graph)
      found.forall(graph(_).size > 2) && (graph.keySet -- found).forall(graph(_).size <= 2)
    )
  }
