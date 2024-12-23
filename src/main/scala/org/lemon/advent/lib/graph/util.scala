package org.lemon.advent.lib.graph

import scala.collection.IterableOps

private def unitAdjacency[N, C[x] <: Iterable[x]](adjacency: N => IterableOps[N, C, _]): N => C[(N, Int)] =
  node => adjacency(node).map((_, 1))
