package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.*
import org.scalacheck.Prop.*

class AllShortestPathsTest extends UnitTest:
  test("allShortestPaths finds exactly the shortest paths") {
    check(forAll(queryGen(maxSize = 7, maxEdgesPerNode = 3)) { case Query(graph, start, end) =>
      val all = graph.simplePaths(start, end)
      val shortest = all.filter(_.distance == all.map(_.distance).minOption.getOrElse(0L)).toSet
      allShortestPaths(graph.adjacency, start, _ == end).map(p => p.copy(distance = p.distance.toLong)) == shortest
    })
  }
