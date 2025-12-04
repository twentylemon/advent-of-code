package org.lemon.advent.lib.graph

/** Compresses a graph by keeping only the specified important nodes and computing
  * shortest path distances between them. Only nodes that are directly connected
  * via corridor paths (through non-important nodes) will have edges in the result.
  * Useful for graphs with many corridor-like paths that can be reduced to weighted
  * edges between junction nodes.
  *
  * @param adjacency function to return edges for a given node
  * @param nodes the important nodes to keep in the compressed graph
  * @return a weighted graph containing only the important nodes with edges weighted by shortest path distances
  * @tparam N the node type
  * @tparam D the distance type
  */
def compress[N, D: Numeric](adjacency: N => Iterable[(N, D)], nodes: Set[N]): WeightedGraph[N, D] =
  def corridorAdjacency(target: N)(node: N): Iterable[(N, D)] =
    adjacency(node).filter((n, _) => n == target || !nodes.contains(n))

  def edgesFrom(start: N): Iterable[(N, D)] =
    (nodes - start)
      .flatMap(end => pathFind(corridorAdjacency(end), start, end).map(path => (end, path.distance)))

  nodes.map(node => node -> edgesFrom(node)).toMap

/** Compresses a graph by keeping only the specified important nodes and computing
  * shortest path distances between them. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param nodes the important nodes to keep in the compressed graph
  * @return a weighted graph containing only the important nodes with edges weighted by shortest path distances
  * @tparam N the node type
  */
def compress[N](adjacency: N => Iterable[N], nodes: Set[N]): WeightedGraph[N, Int] =
  compress(unitAdjacency(adjacency), nodes)

/** Compresses a graph by keeping only the specified important nodes and computing
  * shortest path distances between them.
  *
  * @param graph the graph to compress
  * @param nodes the important nodes to keep in the compressed graph
  * @return a weighted graph containing only the important nodes with edges weighted by shortest path distances
  * @tparam N the node type
  * @tparam D the distance type
  */
def compress[N, D: Numeric](graph: WeightedGraph[N, D], nodes: Set[N]): WeightedGraph[N, D] =
  compress(graph.apply, nodes)

/** Compresses a graph by keeping only the specified important nodes and computing
  * shortest path distances between them. The distance between each node is assumed to be one.
  *
  * @param graph the graph to compress
  * @param nodes the important nodes to keep in the compressed graph
  * @return a weighted graph containing only the important nodes with edges weighted by shortest path distances
  * @tparam N the node type
  */
def compress[N](graph: UnitGraph[N], nodes: Set[N]): WeightedGraph[N, Int] =
  compress(graph.apply, nodes)

/** Finds junction nodes in the graph - nodes with more than two connections.
  * These are typically the important nodes to keep when compressing corridor-like graphs.
  *
  * @param graph the graph to find junctions in
  * @return the set of junction nodes
  * @tparam N the node type
  */
def junctions[N](graph: Map[N, Iterable[?]]): Set[N] =
  graph.filter(_._2.size > 2).keySet
