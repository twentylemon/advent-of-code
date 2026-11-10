package org.lemon.advent.lib.graph

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.math.Numeric.Implicits.*
import scala.math.Ordering.Implicits.*

/** Performs a fill of the graph from the starting node, returning
  * the set of all reachable nodes.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @return the set of reachable nodes from `start`
  * @tparam N the node type
  */
def fill[N](adjacency: N => Iterable[N] | Iterator[N], start: N): Set[N] =
  @tailrec
  def loop(stack: List[N], nodes: Set[N]): Set[N] = stack match
    case Nil => nodes
    case node :: rest =>
      val (nextStack, nextNodes) = adjacency(node).iterator.foldLeft((rest, nodes)) { case ((stack, nodes), neigh) =>
        if nodes(neigh) then (stack, nodes) else (neigh :: stack, nodes + neigh)
      }
      loop(nextStack, nextNodes)

  loop(List(start), Set(start))

/** Performs a breadth first fill of the graph from the starting node, optionally up to a maximum distance,
  * returning the distance to all reachable nodes within that limit.
  *
  * @param adjacency function to return edges for a given node
  * @param end the node to calculate distances to
  * @param maxDistance the maximum distance to explore, or `None` to explore every reachable node
  * @return the map of reachable nodes to their distances from `end`
  * @tparam N the node type
  * @tparam D the distance type
  */
private def distanceFrom[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    end: N,
    maxDistance: Option[D],
): Map[N, D] =
  @tailrec
  def loop(queue: Queue[N], distances: Map[N, D]): Map[N, D] = queue.dequeueOption match
    case None => distances
    case Some((node, rest)) =>
      val dist = distances(node)
      val (nextQueue, nextDistances) = adjacency(node).iterator.foldLeft((rest, distances)) {
        case ((queue, distances), (neigh, d)) =>
          if maxDistance.forall(dist + d <= _) && distances.get(neigh).forall(_ > dist + d) then
            (queue.enqueue(neigh), distances.updated(neigh, dist + d))
          else (queue, distances)
      }
      loop(nextQueue, nextDistances)

  loop(Queue(end), Map(end -> Numeric[D].zero))

/** Performs a breadth first fill of the graph from the starting node up to a maximum distance,
  * returning the distance to all reachable nodes within that limit.
  *
  * @param adjacency function to return edges for a given node
  * @param end the node to calculate distances to
  * @param maxDistance the maximum distance to explore
  * @return the map of reachable nodes to their distances from `end`
  * @tparam N the node type
  * @tparam D the distance type
  */
def distanceFrom[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    end: N,
    maxDistance: D,
): Map[N, D] = distanceFrom(adjacency, end, Some(maxDistance))

/** Performs a breadth first fill of the graph from the starting node, returning
  * the distance to all reachable nodes.
  *
  * @param adjacency function to return edges for a given node
  * @param end the node to calculate distances to
  * @return the set of reachable nodes from `end`
  * @tparam N the node type
  * @tparam D the distance type
  */
def distanceFrom[N, D: Numeric](adjacency: N => Iterable[(N, D)] | Iterator[(N, D)], end: N): Map[N, D] =
  distanceFrom(adjacency, end, None)

/** Performs a breadth first fill of the graph from the starting node, returning
  * the distance to all reachable nodes. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node
  * @param end the node to calculate distances to
  * @return the map of reachable nodes to their distances from `end`
  * @tparam N the node type
  */
def distanceFrom[N](adjacency: N => Iterable[N] | Iterator[N], end: N): Map[N, Int] =
  distanceFrom(unitAdjacency(adjacency), end)

/** Performs a breadth first fill of the graph from the starting node up to a maximum distance,
  * returning the distance to all reachable nodes within that limit. The distance between
  * each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node
  * @param end the node to calculate distances to
  * @param maxDistance the maximum distance to explore
  * @return the map of reachable nodes to their distances from `end`
  * @tparam N the node type
  */
def distanceFrom[N](adjacency: N => Iterable[N] | Iterator[N], end: N, maxDistance: Int): Map[N, Int] =
  distanceFrom(unitAdjacency(adjacency), end, maxDistance)

/** Performs a breadth first fill of the graph from the starting node to the ending nodes, returning
  * all possible paths between the two sets.
  *
  * Note that if `adjacency` can form loops, this function will not terminate.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the set of all paths from `start` to any of `ends`
  * @tparam N the node type
  */
def allPaths[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    start: N,
    ends: N => Boolean,
): Set[Path[N, D]] =
  @tailrec
  def loop(queue: Queue[Path[N, D]], paths: Set[Path[N, D]]): Set[Path[N, D]] = queue.dequeueOption match
    case None => paths
    case Some((node @ Path(path, distance), rest)) =>
      val next = adjacency(node.at).iterator.map((neigh, dist) => Path(path :+ neigh, distance + dist))
      loop(rest ++ next, if ends(node.at) then paths + node else paths)

  loop(Queue(Path(path = Vector(start), distance = Numeric[D].zero)), Set.empty)

/** Performs a breadth first fill of the graph from the starting node to the ending nodes, returning
  * all possible paths between the two sets. The distance between each node is assumed to be one.
  *
  * Note that if `adjacency` can form loops, this function will not terminate.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the set of all paths from `start` to any of `ends`
  * @tparam N the node type
  */
def allPaths[N](adjacency: N => Iterable[N] | Iterator[N], start: N, ends: N => Boolean): Set[Path[N, Int]] =
  allPaths(unitAdjacency(adjacency), start, ends)
