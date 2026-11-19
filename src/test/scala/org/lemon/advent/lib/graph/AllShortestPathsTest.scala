package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.*
import org.scalacheck.Prop.*

class AllShortestPathsTest extends UnitTest:
  test("allShortestPaths finds exactly the shortest paths") {
    check(forAll(queryGen(maxSize = 7, maxEdgesPerNode = 3)) { case Query(graph, start, end) =>
      val all = graph.simplePaths(start, end)
      val shortest = all.filter(_.distance == all.map(_.distance).minOption.getOrElse(0)).toSet
      allShortestPaths(graph.adjacency, start, _ == end) == shortest
    })
  }

  test("allShortestPaths with unit adjacency finds exactly the shortest paths") {
    check(forAll(queryGen(maxSize = 7, maxEdgesPerNode = 3, maxEdgeWeight = 1)) { case Query(graph, start, end) =>
      val all = graph.simplePaths(start, end)
      val shortest = all.filter(_.distance == all.map(_.distance).minOption.getOrElse(0)).toSet
      allShortestPaths(graph.unitAdjacency, start, end) == shortest
    })
  }

  test("allShortestPaths to several ends finds exactly the shortest paths to the closest ones") {
    val gen =
      for
        query <- queryGen(maxSize = 7, maxEdgesPerNode = 3)
        ends <- Gen.someOf(query.graph.nodes)
      yield (query, ends.toSet)
    check(forAll(gen) { case (Query(graph, start, _), ends) =>
      val all = ends.toSeq.flatMap(graph.simplePaths(start, _))
      val shortest = all.filter(_.distance == all.map(_.distance).minOption.getOrElse(0)).toSet
      allShortestPaths(graph.adjacency, start, ends) == shortest
    })
  }

  test("allShortestPaths with unit adjacency to several ends finds exactly the shortest paths to the closest ones") {
    val gen =
      for
        query <- queryGen(maxSize = 7, maxEdgesPerNode = 3, maxEdgeWeight = 1)
        ends <- Gen.someOf(query.graph.nodes)
      yield (query, ends.toSet)
    check(forAll(gen) { case (Query(graph, start, _), ends) =>
      val all = ends.toSeq.flatMap(graph.simplePaths(start, _))
      val shortest = all.filter(_.distance == all.map(_.distance).minOption.getOrElse(0)).toSet
      allShortestPaths(graph.unitAdjacency, start, ends) == shortest
    })
  }
