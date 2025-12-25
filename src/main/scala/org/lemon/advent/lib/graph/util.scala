package org.lemon.advent.lib.graph

private def unitAdjacency[N](adjacency: N => Iterable[N] | Iterator[N]): N => Iterator[(N, Int)] =
  node => adjacency(node).iterator.map((_, 1))
