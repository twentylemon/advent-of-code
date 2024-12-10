package org.lemon.advent.lib.graph

private def unitAdjacency[N](adjacency: N => Seq[N]): N => Seq[(N, Int)] =
  node => adjacency(node).map((_, 1))
