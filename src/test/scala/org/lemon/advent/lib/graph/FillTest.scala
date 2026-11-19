package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.*
import org.scalacheck.Prop.*

class FillTest extends UnitTest:
  test("fill finds every reachable node") {
    check(forAll(queryGen()) { case Query(graph, start, _) =>
      fill(graph.unitAdjacency, start) == graph.distancesFrom(start).keySet
    })
  }

  test("distanceFrom finds the shortest distance to every reachable node") {
    check(forAll(queryGen()) { case Query(graph, start, _) =>
      distanceFrom(graph.adjacency, start) == graph.distancesFrom(start)
    })
  }

  test("distanceFrom finds Long distances beyond Int.MaxValue") {
    val scale = 1_000_000_000L
    check(forAll(queryGen()) { case Query(graph, start, _) =>
      // an iterator, so this only checks the distance limit and not how parallel edges are handled
      val adjacency = (node: Int) => graph.adjacency(node).iterator.map((to, w) => (to, w * scale))
      distanceFrom(adjacency, start) == graph.distancesFrom(start).view.mapValues(_ * scale).toMap
    })
  }

  test("distanceFrom with a maximum distance keeps every node within it") {
    val gen =
      for
        query <- queryGen()
        max <- Gen.choose(0L, query.graph.distancesFrom(query.start).values.max)
      yield (query, max.toInt)
    check(forAll(gen) { case (Query(graph, start, _), max) =>
      distanceFrom(graph.adjacency, start, max) == graph.distancesFrom(start).filter(_._2 <= max)
    })
  }

  test("distanceFrom with unit adjacency counts the steps to every reachable node") {
    check(forAll(queryGen()) { case Query(graph, start, _) =>
      val steps = graph.copy(edges = graph.edges.map((from, to, _) => (from, to, 1)))
      distanceFrom(graph.unitAdjacency, start) == steps.distancesFrom(start)
    })
  }

  test("distanceFrom with unit adjacency and a maximum distance keeps every node within it") {
    val gen =
      for
        query <- queryGen()
        max <- Gen.choose(0, query.graph.size - 1)
      yield (query, max)
    check(forAll(gen) { case (Query(graph, start, _), max) =>
      val steps = graph.copy(edges = graph.edges.map((from, to, _) => (from, to, 1)))
      distanceFrom(graph.unitAdjacency, start, max) == steps.distancesFrom(start).filter(_._2 <= max)
    })
  }

  test("allPaths finds every path to any of the ends in an acyclic graph") {
    val gen =
      for
        query <- queryGen()
        ends <- Gen.someOf(query.graph.nodes)
      yield (query, ends.toSet)
    check(forAll(gen) { case (Query(graph, start, _), ends) =>
      val edges = graph.edges.filter((from, to, _) => from < to).distinctBy((from, to, _) => (from, to))
      val dag = graph.copy(edges = edges)
      allPaths(dag.adjacency, start, ends) == ends.flatMap(dag.simplePaths(start, _))
    })
  }

  test("allPaths with unit adjacency finds every path to any of the ends in an acyclic graph") {
    val gen =
      for
        query <- queryGen()
        ends <- Gen.someOf(query.graph.nodes)
      yield (query, ends.toSet)
    check(forAll(gen) { case (Query(graph, start, _), ends) =>
      val edges = graph.edges.filter((from, to, _) => from < to).map((from, to, _) => (from, to, 1)).distinct
      val dag = graph.copy(edges = edges)
      allPaths(dag.unitAdjacency, start, ends) == ends.flatMap(dag.simplePaths(start, _))
    })
  }
