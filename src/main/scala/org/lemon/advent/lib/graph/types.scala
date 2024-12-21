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

/** The path taken by a search algorithm.
  * @param path the path taken
  * @param distance the distance travelled
  * @tparam N the node type
  * @tparam D the distance type
  */
case class Path[N, D](path: Seq[N], distance: D):
  def at = path.last
