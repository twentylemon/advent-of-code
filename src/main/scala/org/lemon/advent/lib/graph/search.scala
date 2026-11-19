package org.lemon.advent.lib.graph

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.math.Numeric.Implicits.*
import scala.math.Ordering.Implicits.*

/** Performs an A* search of the graph from any of the `starts`, returning
  * the shortest path from one of them to the closest end. When `heuristic` always returns zero,
  * this is equivalent to dijkstra's search.
  *
  * @param adjacency function to return edges for a given node
  * @param heuristic function estimating the remaining cost from a node to the goal (must be admissible)
  * @param starts the start nodes
  * @param ends function to check if a node is an ending node
  * @return the shortest path from any start to the closest end, or empty if no end is reachable
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    heuristic: N => D,
    starts: Iterable[N],
    ends: N => Boolean,
): Option[Path[N, D]] =
  @tailrec
  def unwind(best: Map[N, (D, N)], node: N, path: List[N]): Vector[N] =
    val parent = best(node)._2
    if parent == node then (node :: path).toVector else unwind(best, parent, node :: path)

  @tailrec
  def search(queue: MinQueue[D, (D, N)], best: Map[N, (D, N)]): Option[Path[N, D]] =
    queue.dequeue match
      case None => None
      case Some((_, (cost, node), rest)) if cost > best(node)._1 => search(rest, best)
      case Some((_, (cost, node), _)) if ends(node) => Some(Path(unwind(best, node, Nil), cost))
      case Some((_, (cost, node), rest)) =>
        val (nextQueue, nextBest) = adjacency(node).iterator.foldLeft((rest, best)) {
          case (acc @ (queue, best), (next, dist)) =>
            val nextCost = cost + dist
            if best.get(next).exists(_._1 <= nextCost) then acc
            else (queue.enqueue(nextCost + heuristic(next), (nextCost, next)), best.updated(next, (nextCost, node)))
        }
        search(nextQueue, nextBest)

  val zero = Numeric[D].zero
  val queue = starts.foldLeft(MinQueue[D, (D, N)](TreeMap.empty))((queue, start) =>
    queue.enqueue(heuristic(start), (zero, start))
  )
  search(queue, starts.iterator.map(start => start -> (zero, start)).toMap)

/** Performs an A* search of the graph from `start`, returning
  * the shortest path to the closest end. When `heuristic` always returns zero,
  * this is equivalent to dijkstra's search.
  *
  * @param adjacency function to return edges for a given node
  * @param heuristic function estimating the remaining cost from a node to the goal (must be admissible)
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the shortest path from `start` to the closest end, or empty if no end is reachable
  * @tparam N the node type
  * @tparam D the distance type
  */
def pathFind[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    heuristic: N => D,
    start: N,
    ends: N => Boolean,
): Option[Path[N, D]] =
  pathFind(adjacency, heuristic, Seq(start), ends)

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

/** Performs an A* search of the graph from `start`, returning
  * the shortest path to the closest end. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param heuristic function estimating the remaining cost from a node to the goal (must be admissible)
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the shortest path from `start` to the closest end, or empty if no end is reachable
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
  pathFind(adjacency, heuristic, start, end == _)

/** Performs a dijkstra's search of the graph from `start`, returning
  * the shortest path to the closest end.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the shortest path from `start` to the closest end, or empty if no end is reachable
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

/** Performs a dijkstra's search of the graph from `start`, returning
  * the shortest path to the closest end. The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return the shortest path from `start` to the closest end, or empty if no end is reachable
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
  pathFind(adjacency, start, end == _)

/** Finds all shortest paths in the graph from `start` to the closest ends. Edge weights must be positive.
  *
  * @param adjacency function to return edges for a given node
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return every shortest path from `start` to an end at the shortest distance
  * @tparam N the node type
  * @tparam D the distance type
  */
def allShortestPaths[N, D: Numeric](
    adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
    start: N,
    ends: N => Boolean,
): Set[Path[N, D]] =
  // dijkstra that remembers every equally cheap parent of each node, stopping once past the closest end
  case class Search(best: Map[N, (D, Set[N])], shortest: Option[D], endsFound: Set[N])

  @tailrec
  def search(queue: MinQueue[D, N], state: Search): Search =
    queue.dequeue match
      case Some((cost, node, rest)) if state.shortest.forall(cost <= _) =>
        if cost > state.best(node)._1 then search(rest, state)
        else if ends(node) then search(rest, state.copy(shortest = Some(cost), endsFound = state.endsFound + node))
        else
          val (nextQueue, nextState) = adjacency(node).iterator.foldLeft((rest, state)) {
            case (acc @ (queue, state), (next, dist)) =>
              val nextCost = cost + dist
              state.best.get(next) match
                case Some((known, _)) if nextCost > known => acc
                case Some((known, parents)) if nextCost == known =>
                  (queue, state.copy(best = state.best.updated(next, (known, parents + node))))
                case _ =>
                  (queue.enqueue(nextCost, next), state.copy(best = state.best.updated(next, (nextCost, Set(node)))))
          }
          search(nextQueue, nextState)
      case _ => state

  val zero = Numeric[D].zero
  val result = search(MinQueue(zero, start), Search(Map(start -> (zero, Set.empty)), None, Set.empty))

  @tailrec
  def unwindAll(stack: List[(N, List[N])], paths: Set[Vector[N]]): Set[Vector[N]] = stack match
    case Nil => paths
    case (node, suffix) :: rest if node == start => unwindAll(rest, paths + (node :: suffix).toVector)
    case (node, suffix) :: rest =>
      unwindAll(result.best(node)._2.foldLeft(rest)((stack, parent) => (parent, node :: suffix) :: stack), paths)

  result.shortest match
    case Some(distance) => unwindAll(result.endsFound.toList.map((_, List.empty[N])), Set.empty).map(Path(_, distance))
    case None => Set.empty

/** Finds all shortest paths in the graph from `start` to the closest ends.
  * The distance between each node is assumed to be one.
  *
  * @param adjacency function to return edges for a given node, all with distance one
  * @param start the start node
  * @param ends function to check if a node is an ending node
  * @return every shortest path from `start` to an end at the shortest distance
  * @tparam N the node type
  */
def allShortestPaths[N](adjacency: N => Iterable[N] | Iterator[N], start: N, ends: N => Boolean): Set[Path[N, Int]] =
  allShortestPaths(unitAdjacency(adjacency), start, ends)
