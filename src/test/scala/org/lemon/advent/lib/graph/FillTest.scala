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
