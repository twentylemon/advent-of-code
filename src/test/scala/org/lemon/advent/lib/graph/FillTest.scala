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
      distanceFrom(graph.adjacency, start).view.mapValues(_.toLong).toMap == graph.distancesFrom(start)
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
        max <- Gen.choose(0, 60)
      yield (query.graph.copy(edges = query.graph.edges.map((from, to, w) => (from, to, w % 5))), query.start, max)
    check(forAll(gen) { (graph, start, max) =>
      distanceFrom(graph.adjacency, start, max).view.mapValues(_.toLong).toMap ==
        graph.distancesFrom(start).filter(_._2 <= max)
    })
  }

  test("distanceFrom with unit adjacency counts the steps to every reachable node") {
    check(forAll(queryGen()) { case Query(graph, start, _) =>
      val steps = graph.copy(edges = graph.edges.map((from, to, _) => (from, to, 1)))
      distanceFrom(graph.unitAdjacency, start).view.mapValues(_.toLong).toMap == steps.distancesFrom(start)
    })
  }

  test("distanceFrom with unit adjacency and a maximum distance keeps every node within it") {
    check(forAll(queryGen(), Gen.choose(0, 8)) { (query, max) =>
      val steps = query.graph.copy(edges = query.graph.edges.map((from, to, _) => (from, to, 1)))
      distanceFrom(query.graph.unitAdjacency, query.start, max).view.mapValues(_.toLong).toMap ==
        steps.distancesFrom(query.start).filter(_._2 <= max)
    })
  }

  test("allPaths finds every path in an acyclic graph") {
    check(forAll(queryGen()) { case Query(graph, start, end) =>
      val dag =
        graph.copy(edges = graph.edges.filter((from, to, _) => from < to).distinctBy((from, to, _) => (from, to)))
      allPaths(dag.adjacency, start, _ == end).map(p => p.copy(distance = p.distance.toLong)) ==
        dag.simplePaths(start, end).toSet
    })
  }

  test("allPaths with unit adjacency finds every path in an acyclic graph") {
    check(forAll(queryGen()) { case Query(graph, start, end) =>
      val dag = graph.copy(edges =
        graph.edges.filter((from, to, _) => from < to).distinctBy((from, to, _) => (from, to)).map((f, t, _) =>
          (f, t, 1)
        )
      )
      allPaths(dag.unitAdjacency, start, _ == end).map(p => p.copy(distance = p.distance.toLong)) ==
        dag.simplePaths(start, end).toSet
    })
  }
