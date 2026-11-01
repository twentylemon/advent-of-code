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
  given Ordering[(D, Path[N, D])] = Ordering.by[(D, Path[N, D]), D](_._1).reverse
  val queue = mutable.PriorityQueue((heuristic(start), Path(Vector(start), Numeric[D].zero)))
  val visited = mutable.Set.empty[N]

  while queue.nonEmpty && !ends(queue.head._2.at) do
    val (_, Path(path, g)) = queue.dequeue()
    if visited.add(path.last) then
      queue ++= adjacency(path.last)
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

/** Finds all shortest paths in the graph from `start` to `end`. Edge weights must be positive.
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
  // dijkstra that remembers every equally cheap parent of each node, stopping once past the closest end
  case class Search(best: Map[N, D], parents: Map[N, Set[N]], shortest: Option[D], endsFound: Set[N])

  @tailrec
  def search(queue: MinQueue[D, N], state: Search): Search =
    queue.dequeue match
      case Some((cost, node, rest)) if state.shortest.forall(cost <= _) =>
        if cost > state.best(node) then search(rest, state)
        else if ends(node) then search(rest, state.copy(shortest = Some(cost), endsFound = state.endsFound + node))
        else
          val (nextQueue, nextState) = adjacency(node).iterator.foldLeft((rest, state)) {
            case (acc @ (queue, state), (next, dist)) =>
              val nextCost = cost + dist
              state.best.get(next) match
                case Some(known) if nextCost > known => acc
                case Some(known) if nextCost == known =>
                  (queue, state.copy(parents = state.parents.updated(next, state.parents(next) + node)))
                case _ =>
                  (
                    queue.enqueue(nextCost, next),
                    state.copy(best = state.best.updated(next, nextCost), parents = state.parents.updated(next, Set(node))),
                  )
          }
          search(nextQueue, nextState)
      case _ => state

  val zero = Numeric[D].zero
  val result = search(MinQueue(zero, start), Search(Map(start -> zero), Map.empty, None, Set.empty))

  def pathsTo(node: N): Iterator[Vector[N]] =
    if node == start then Iterator(Vector(start))
    else result.parents(node).iterator.flatMap(parent => pathsTo(parent).map(_ :+ node))

  result.shortest match
    case Some(distance) => result.endsFound.iterator.flatMap(pathsTo).map(Path(_, distance)).toSet
    case None => Set.empty

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
