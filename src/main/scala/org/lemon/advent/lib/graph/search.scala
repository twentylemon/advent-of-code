package org.lemon.advent.lib.graph

import scala.collection.mutable
import scala.math.Numeric.Implicits.*
import scala.math.Ordering.Implicits.*

/** Performs an A* search of the graph from `start` to `end`, returning
  * the shortest path between them. When `heuristic` always returns zero,
  * this is equivalent to dijkstra's search.
  *
  * @param adjacency function to return edges for a given node
  * @param heuristic function estimating the remaining cost from a node to the goal (must be admissible)
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D: Numeric](
    adjacency: N => Iterable[(N, D)],
    heuristic: N => D,
    start: N,
    ends: N => Boolean,
): Option[Path[N, D]] =
  given Ordering[(D, Path[N, D])] = Ordering.by[(D, Path[N, D]), D](_._1).reverse
  val queue = mutable.PriorityQueue((heuristic(start), Path(Vector(start), Numeric[D].zero)))
  val visited = mutable.Set(start)

  while queue.nonEmpty && !ends(queue.head._2.at) do
    val (_, Path(path, g)) = queue.dequeue()
    queue ++= adjacency(path.last)
      .filter((neigh, _) => visited.add(neigh))
      .map((neigh, dist) => (g + dist + heuristic(neigh), Path(path :+ neigh, g + dist)))

  queue.headOption.map(_._2)

/** Performs an A* search of the graph from `start` to `end`, returning
  * the shortest path between them.
  *
  * @param adjacency function to return edges for a given node
  * @param heuristic function estimating the remaining cost from a node to the goal (must be admissible)
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D: Numeric](adjacency: N => Iterable[(N, D)], heuristic: N => D, start: N, end: N): Option[Path[N, D]] =
  pathFind(adjacency, heuristic, start, end == _)

/** Performs an A* search of the graph from `start` to `end`, returning
  * the shortest path between them. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param heuristic function estimating the remaining cost from a node to the goal (must be admissible)
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  */
def pathFind[N](adjacency: N => Iterable[N], heuristic: N => Int, start: N, ends: N => Boolean): Option[Path[N, Int]] =
  pathFind(unitAdjacency(adjacency), heuristic, start, ends)

/** Performs an A* search of the graph from `start` to `end`, returning
  * the shortest path between them. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param heuristic function estimating the remaining cost from a node to the goal (must be admissible)
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  */
def pathFind[N](adjacency: N => Iterable[N], heuristic: N => Int, start: N, end: N): Option[Path[N, Int]] =
  pathFind(unitAdjacency(adjacency), heuristic, start, end)

/** Performs an A* search of the graph from `start` to `end`, returning
  * the shortest path between them.
  *
  * @param graph the graph to search in
  * @param heuristic function estimating the remaining cost from a node to the goal (must be admissible)
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D: Numeric](graph: WeightedGraph[N, D], heuristic: N => D, start: N, end: N): Option[Path[N, D]] =
  pathFind(graph.apply, heuristic, start, end)

/** Performs an A* search of the graph from `start` to `end`, returning
  * the shortest path between them. The distance between each node is assumed to be one.
  *
  * @param graph the graph to search in
  * @param heuristic function estimating the remaining cost from a node to the goal (must be admissible)
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  */
def pathFind[N](graph: UnitGraph[N], heuristic: N => Int, start: N, end: N): Option[Path[N, Int]] =
  pathFind(graph.apply, heuristic, start, end)

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D: Numeric](adjacency: N => Iterable[(N, D)], start: N, ends: N => Boolean): Option[Path[N, D]] =
  pathFind(adjacency, _ => Numeric[D].zero, start, ends)

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D: Numeric](adjacency: N => Iterable[(N, D)], start: N, end: N): Option[Path[N, D]] =
  pathFind(adjacency, start, end == _)

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  */
def pathFind[N](adjacency: N => Iterable[N], start: N, ends: N => Boolean): Option[Path[N, Int]] =
  pathFind(unitAdjacency(adjacency), start, ends)

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  */
def pathFind[N](adjacency: N => Iterable[N], start: N, end: N): Option[Path[N, Int]] =
  pathFind(unitAdjacency(adjacency), start, end)

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them.
  *
  * @param graph the graph to search in
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D: Numeric](graph: WeightedGraph[N, D], start: N, end: N): Option[Path[N, D]] =
  pathFind(graph.apply, start, end)

/** Performs a dijkstra's search of the graph from `start` to `end`, returning
  * the shortest path between them. The distance between each node is assumed to be one.
  *
  * @param graph the graph to search in
  * @param start the start node
  * @param end the end node
  * @return the shortest path between `start` and `end`, or empty if no path exists
  * @tparam N the node type
  */
def pathFind[N](graph: UnitGraph[N], start: N, end: N): Option[Path[N, Int]] =
  pathFind(graph.apply, start, end)

/** Finds all shortest paths in the graph from `start` to `end`.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return set of all shortest paths between `start` and `end`
  * @tparam N the node type
  * @tparam D the distance type
  */
def allShortestPaths[N, D: Numeric](adjacency: N => Iterable[(N, D)], start: N, ends: N => Boolean): Set[Path[N, D]] =
  val paths = mutable.Set.empty[Path[N, D]]
  val queue = mutable.Queue(Path(path = Vector(start), distance = Numeric[D].zero))
  val costs = mutable.Map(start -> Numeric[D].zero)

  while !queue.isEmpty do
    val node @ Path(path, distance) = queue.dequeue
    if ends(node.at) then
      if paths.isEmpty || distance < paths.head.distance then paths.clear()
      if paths.isEmpty || distance <= paths.head.distance then paths.add(node)

    queue ++= adjacency(node.at)
      .filter((neigh, dist) =>
        val costTo = distance + dist
        costs.get(neigh) match
          case Some(known) if known < costTo => false
          case _ => costs(neigh) = costTo; true
      )
      .map((neigh, dist) => Path(path :+ neigh, distance + dist))

  paths.toSet

/** Finds all shortest paths in the graph from `start` to `end`.
  * The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return set of all shortest paths between `start` and `end`
  * @tparam N the node type
  */
def allShortestPaths[N](adjacency: N => Iterable[N], start: N, ends: N => Boolean): Set[Path[N, Int]] =
  allShortestPaths(unitAdjacency(adjacency), start, ends)
