package org.lemon.advent.lib.graph

import org.lemon.advent.*
import org.lemon.advent.lib.*
import org.scalacheck.*
import org.scalacheck.Prop.*

class CompressTest extends UnitTest:
  test("compress keeps the shortest corridor distance between each pair of important nodes") {
    val gen =
      for
        graph <- graphGen(maxSize = 8, maxEdgesPerNode = 3)
        important <- Gen.someOf(graph.nodes)
      yield (graph, important.toSet)

    check(forAll(gen) { (graph: Graph, important: Set[Int]) =>
      val compressed = compress(graph.adjacency, important)
      important.forall { from =>
        // can leave `from` and pass through unimportant nodes, but stops at the first other important node
        val corridors = graph.copy(edges = graph.edges.filter((f, t, _) => (f == from || !important(f)) && t != from))
        val distances = corridors.distancesFrom(from)
        val expected = (important - from).flatMap(to => distances.get(to).map(to -> _))
        compressed(from).map((to, d) => (to, d.toLong)).toSet == expected
      }
    })
  }
