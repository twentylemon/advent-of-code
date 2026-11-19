package org.lemon.advent.lib.graph

import org.lemon.advent.lib.*

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
      val (nextStack, nextNodes) = adjacency(node).foldLeft((rest, nodes)) { case ((stack, nodes), neigh) =>
        if nodes(neigh) then (stack, nodes) else (neigh :: stack, nodes + neigh)
      }
      loop(nextStack, nextNodes)

  loop(List(start), Set(start))

/** Performs a breadth first fill of the graph from the starting node, optionally up to a maximum distance,
  * returning the distance to all reachable nodes within that limit.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param maxDistance the maximum distance to explore, or `None` to explore every reachable node
  * @return the map of reachable nodes to their distances from `start`
  * @tparam N the node type
  * @tparam D the distance type
  */
private def distanceFrom[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    start: N,
    maxDistance: Option[D],
): Map[N, D] =
  @tailrec
  def loop(queue: Queue[N], distances: Map[N, D]): Map[N, D] = queue.dequeueOption match
    case None => distances
    case Some((node, rest)) =>
      val dist = distances(node)
      val (nextQueue, nextDistances) = adjacency(node).foldLeft((rest, distances)) {
        case ((queue, distances), (neigh, d)) =>
          if maxDistance.forall(dist + d <= _) && distances.get(neigh).forall(_ > dist + d) then
            (queue.enqueue(neigh), distances.updated(neigh, dist + d))
          else (queue, distances)
      }
      loop(nextQueue, nextDistances)

  loop(Queue(start), Map(start -> Numeric[D].zero))

/** Performs a breadth first fill of the graph from the starting node up to a maximum distance,
  * returning the distance to all reachable nodes within that limit.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param maxDistance the maximum distance to explore
  * @return the map of reachable nodes to their distances from `start`
  * @tparam N the node type
  * @tparam D the distance type
  */
def distanceFrom[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    start: N,
    maxDistance: D,
): Map[N, D] = distanceFrom(adjacency, start, Some(maxDistance))

/** Performs a breadth first fill of the graph from the starting node, returning
  * the distance to all reachable nodes.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @return the map of reachable nodes to their distances from `start`
  * @tparam N the node type
  * @tparam D the distance type
  */
def distanceFrom[N, D: Numeric](adjacency: N => Iterable[(N, D)] | Iterator[(N, D)], start: N): Map[N, D] =
  distanceFrom(adjacency, start, None)

/** Performs a breadth first fill of the graph from the starting node, returning
  * the distance to all reachable nodes. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @return the map of reachable nodes to their distances from `start`
  * @tparam N the node type
  */
def distanceFrom[N](adjacency: N => Iterable[N] | Iterator[N], start: N): Map[N, Int] =
  distanceFrom(unitAdjacency(adjacency), start)

/** Performs a breadth first fill of the graph from the starting node up to a maximum distance,
  * returning the distance to all reachable nodes within that limit. The distance between
  * each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param maxDistance the maximum distance to explore
  * @return the map of reachable nodes to their distances from `start`
  * @tparam N the node type
  */
def distanceFrom[N](adjacency: N => Iterable[N] | Iterator[N], start: N, maxDistance: Int): Map[N, Int] =
  distanceFrom(unitAdjacency(adjacency), start, maxDistance)

/** Performs a breadth first fill of the graph from the starting node to the ending nodes, returning
  * all possible paths between them.
  *
  * Note that if `adjacency` can form loops, this function will not terminate.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the set of all paths from `start` to any of `ends`
  * @tparam N the node type
  * @tparam D the distance type
  */
def allPaths[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    start: N,
    ends: N => Boolean,
): Set[Path[N, D]] =
  @tailrec
  def loop(queue: Queue[Path[N, D]], paths: Set[Path[N, D]]): Set[Path[N, D]] = queue.dequeueOption match
    case None => paths
    case Some((path @ Path(nodes, distance), rest)) =>
      val next = adjacency(path.at).map((neigh, dist) => Path(nodes :+ neigh, distance + dist))
      loop(rest ++ next, if ends(path.at) then paths + path else paths)

  loop(Queue(Path(path = Vector(start), distance = Numeric[D].zero)), Set.empty)

/** Performs a breadth first fill of the graph from the starting node to the ending nodes, returning
  * all possible paths between them. The distance between each node is assumed to be one.
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

/** Counts the paths from the starting node to the ending nodes, the same as `allPaths(adjacency, start, ends).size`
  * without building the paths.
  *
  * Note that if `adjacency` can form loops, this function will blow up.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the number of paths from `start` to any of `ends`
  * @tparam N the node type
  */
def countPaths[N](adjacency: N => Iterable[N] | Iterator[N], start: N, ends: N => Boolean): Long =
  lazy val count: N => Long = memoize { node =>
    adjacency(node).iterator.map(count).sum + (if ends(node) then 1 else 0)
  }
  count(start)

/** Counts the paths from the starting node to the ending node, the same as `allPaths(adjacency, start, _ == end).size`
  * without building the paths.
  *
  * Note that if `adjacency` can form loops, this function will blow up.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param end the end node
  * @return the number of paths from `start` to `end`
  * @tparam N the node type
  */
def countPaths[N](adjacency: N => Iterable[N] | Iterator[N], start: N, end: N): Long =
  countPaths(adjacency, start, end == _)
