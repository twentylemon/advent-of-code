package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.*
import org.scalacheck.Prop.*

class PathFindTest extends UnitTest:
  test("pathFind finds the shortest distance") {
    check(forAll(queryGen()) { case Query(graph, start, end) =>
      pathFind(graph.adjacency, start, end).map(_.distance.toLong) == graph.distancesFrom(start).get(end)
    })
  }

  test("pathFind returns a path along real edges with the reported distance") {
    check(forAll(queryGen()) { case Query(graph, start, end) =>
      pathFind(graph.adjacency, start, end).forall(found =>
        val edgeCosts = found.path.sliding2.map((a, b) => graph.adjacency(a).filter(_._1 == b).map(_._2).min)
        found.path.head == start && found.at == end && edgeCosts.sum == found.distance
      )
    })
  }

  test("pathFind with an admissible heuristic finds the shortest distance") {
    // scaling down the true remaining distance never overestimates, so it is admissible (but maybe not consistent)
    // 0 -> 1 -> 2 is a detour next to a direct 0 -> 2 edge, and 2 leads on to the end at 3. when the
    // detour is cheaper, but node 1 looks worse to the heuristic than node 2, node 2 is reached by the
    // direct edge first
    val detour =
      for
        weights <- Gen.listOfN(4, Gen.choose(1, 20))
        rest <- graphGen(maxSize = 4, maxEdgesPerNode = 1)
      yield Query(
        Graph(4, List((0, 1, weights(0)), (1, 2, weights(1)), (0, 2, weights(2)), (2, 3, weights(3))) ++ rest.edges),
        start = 0,
        end = 3,
      )
    val gen =
      for
        query <- Gen.oneOf(queryGen(), detour)
        scales <- Gen.listOfN(query.graph.size, Gen.oneOf(Gen.const(0.0), Gen.const(1.0), Gen.choose(0.0, 1.0)))
      yield (query, scales)

    check(forAll(gen) { case (Query(graph, start, end), scales) =>
      val toEnd = graph.reversed.distancesFrom(end)
      val heuristic = (node: Int) => toEnd.get(node).map(d => (d * scales(node)).toInt).getOrElse(0)
      pathFind(graph.adjacency, heuristic, start, end).map(_.distance.toLong) == graph.distancesFrom(start).get(end)
    })
  }
