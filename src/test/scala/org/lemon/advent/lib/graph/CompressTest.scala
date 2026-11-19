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

  def corridorDistances(graph: Graph, important: Set[Int]): Map[Int, Set[(Int, Int)]] =
    important.map { from =>
      // can leave `from` and pass through unimportant nodes, but stops at the first other important node
      val corridors = graph.copy(edges = graph.edges.filter((f, t, _) => (f == from || !important(f)) && t != from))
      val distances = corridors.distancesFrom(from)
      from -> (important - from).flatMap(to => distances.get(to).map(to -> _))
    }.toMap

  test("compress keeps the shortest corridor distance between each pair of important nodes") {
    check(forAll(gen) { (graph: Graph, important: Set[Int]) =>
      compress(graph.adjacency, important).view.mapValues(_.toSet).toMap == corridorDistances(graph, important)
    })
  }

  test("compress of a unit graph counts the steps between each pair of important nodes") {
    check(forAll(gen) { (graph: Graph, important: Set[Int]) =>
      val unitGraph = graph.nodes.map(node => node -> graph.unitAdjacency(node)).toMap
      val steps = graph.copy(edges = graph.edges.map((from, to, _) => (from, to, 1)))
      compress(unitGraph, important).view.mapValues(_.toSet).toMap == corridorDistances(steps, important)
    })
  }

  test("junctions are the nodes with more than two neighbours") {
    check((graph: Map[Int, List[Int]]) =>
      val found = junctions(graph)
      found.forall(graph(_).size > 2) && (graph.keySet -- found).forall(graph(_).size <= 2)
    )
  }
