package org.lemon.advent.lib.graph

/** Static adjacency list. A map of `node => [(neighbour, distance)...]`
  * @tparam N the node type
  * @tparam D the distance type
  */
type WeightedGraph[N, D] = Map[N, Seq[(N, D)]]

/** Static adjacency list. A map of `node => [neighbour...]`. The distances between all
  * nodes is assumed to be one.
  * @tparam N the node type
  */
type UnitGraph[N] = Map[N, Seq[N]]
