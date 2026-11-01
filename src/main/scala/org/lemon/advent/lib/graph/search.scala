package org.lemon.advent.lib.graph

import scala.annotation.tailrec
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
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    heuristic: N => D,
    start: N,
    ends: N => Boolean,
): Option[Path[N, D]] =
  @tailrec
  def search(queue: MinQueue[D, (D, N)], best: Map[N, D], parent: Map[N, N]): Option[Path[N, D]] =
    queue.dequeue match
      case None => None
      // a node is queued again whenever a cheaper route to it is found, which can be after it was
      // expanded when the heuristic is admissible but not consistent. skip the stale entries
      case Some((_, (cost, node), rest)) if cost > best(node) => search(rest, best, parent)
      case Some((_, (cost, node), _)) if ends(node) => Some(Path(unwind(parent, node), cost))
      case Some((_, (cost, node), rest)) =>
        val (nextQueue, nextBest, nextParent) = adjacency(node).iterator.foldLeft((rest, best, parent)) {
          case (acc @ (queue, best, parent), (next, dist)) =>
            val nextCost = cost + dist
            if best.get(next).exists(_ <= nextCost) then acc
            else
              (
                queue.enqueue(nextCost + heuristic(next), (nextCost, next)),
                best.updated(next, nextCost),
                parent.updated(next, node),
              )
        }
        search(nextQueue, nextBest, nextParent)

  val zero = Numeric[D].zero
  search(MinQueue(heuristic(start), (zero, start)), Map(start -> zero), Map.empty)

/** @return the path from the root of the `parent` tree down to `end` */
private def unwind[N](parent: Map[N, N], end: N): Vector[N] =
  (end :: List.unfold(end)(node => parent.get(node).map(p => (p, p)))).reverse.toVector

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
def pathFind[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    heuristic: N => D,
    start: N,
    end: N,
): Option[Path[N, D]] =
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
def pathFind[N](
    adjacency: N => Iterable[N] | Iterator[N],
    heuristic: N => Int,
    start: N,
    ends: N => Boolean,
): Option[Path[N, Int]] =
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
def pathFind[N](
    adjacency: N => Iterable[N] | Iterator[N],
    heuristic: N => Int,
    start: N,
    end: N,
): Option[Path[N, Int]] =
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
def pathFind[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    start: N,
    ends: N => Boolean,
): Option[Path[N, D]] =
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
def pathFind[N, D: Numeric](adjacency: N => Iterable[(N, D)] | Iterator[(N, D)], start: N, end: N): Option[Path[N, D]] =
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
def pathFind[N](adjacency: N => Iterable[N] | Iterator[N], start: N, ends: N => Boolean): Option[Path[N, Int]] =
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
def pathFind[N](adjacency: N => Iterable[N] | Iterator[N], start: N, end: N): Option[Path[N, Int]] =
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
def allShortestPaths[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    start: N,
    ends: N => Boolean,
): Set[Path[N, D]] =
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
def allShortestPaths[N](adjacency: N => Iterable[N] | Iterator[N], start: N, ends: N => Boolean): Set[Path[N, Int]] =
  allShortestPaths(unitAdjacency(adjacency), start, ends)
