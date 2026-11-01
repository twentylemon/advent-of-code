package org.lemon.advent.bench

import org.lemon.advent.lib.*
import org.lemon.advent.lib.`2d`.*
import org.lemon.advent.lib.graph.{Path, WeightedGraph}

import scala.collection.{AbstractIterator, mutable}
import scala.math.Numeric.Implicits.*
import scala.math.Ordering.Implicits.*

/** Candidate replacements for library functions. Each benchmark checks that a candidate
  * gives the same answer as the current library version before timing them against each other.
  */
object candidates:

  /** Iterates `start..end` without `Iterator.iterate(...).takeWhile(...)`. Also stops at the type's
    * max value, where the current `Interval.iterator` overflows and never ends.
    */
  final class IntervalIterator[N](start: N, end: N)(using num: Integral[N]) extends AbstractIterator[N]:
    private var current = start
    private var more = num.lteq(start, end)
    def hasNext: Boolean = more
    def next(): N =
      if !more then Iterator.empty.next()
      val result = current
      if num.equiv(result, end) then more = false else current = num.plus(result, num.one)
      result

  def rectIterator[N: Integral](rect: Rect[N]): Iterator[Point[N]] =
    for
      y <- IntervalIterator(rect.yRange.start, rect.yRange.end)
      x <- IntervalIterator(rect.xRange.start, rect.xRange.end)
    yield Point(x, y)

  private def edges[N, D](adjacency: N => Iterable[(N, D)] | Iterator[(N, D)], node: N): Iterator[(N, D)] =
    adjacency(node).iterator

  private def unwind[N](parent: collection.Map[N, N], end: N): Vector[N] =
    Iterator.iterate(Option(end))(_.flatMap(parent.get)).takeWhile(_.isDefined).map(_.get).toVector.reverse

  /** A* that only queues a node when it improves the best known cost to it, and keeps parent
    * pointers instead of copying the whole path into every queue entry. Re-expanding a node when a
    * cheaper route turns up also makes it correct for admissible but inconsistent heuristics.
    */
  def pathFind[N, D: Numeric](
      adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
      heuristic: N => D,
      start: N,
      ends: N => Boolean,
  ): Option[Path[N, D]] =
    val best = mutable.HashMap(start -> Numeric[D].zero)
    val parent = mutable.HashMap.empty[N, N]
    given Ordering[(D, D, N)] = Ordering.by[(D, D, N), D](_._1).reverse
    val queue = mutable.PriorityQueue((heuristic(start), Numeric[D].zero, start))
    var found = Option.empty[Path[N, D]]
    while found.isEmpty && queue.nonEmpty do
      val (_, cost, node) = queue.dequeue()
      if cost <= best(node) then // otherwise a cheaper route to node was queued after this one
        if ends(node) then found = Some(Path(unwind(parent, node), cost))
        else
          edges(adjacency, node).foreach { (next, d) =>
            val nextCost = cost + d
            if best.get(next).forall(nextCost < _) then
              best(next) = nextCost
              parent(next) = node
              queue.enqueue((nextCost + heuristic(next), nextCost, next))
          }
    found

  /** One dijkstra per important node that stops at other important nodes, rather than one search
    * per pair of important nodes.
    */
  def compress[N, D: Numeric](adjacency: N => Iterable[(N, D)] | Iterator[(N, D)], nodes: Set[N]): WeightedGraph[N, D] =
    def edgesFrom(start: N): Set[(N, D)] =
      val best = mutable.HashMap(start -> Numeric[D].zero)
      given Ordering[(D, N)] = Ordering.by[(D, N), D](_._1).reverse
      val queue = mutable.PriorityQueue((Numeric[D].zero, start))
      val found = mutable.HashMap.empty[N, D]
      while queue.nonEmpty do
        val (cost, node) = queue.dequeue()
        if cost <= best(node) then
          if node != start && nodes.contains(node) then found(node) = cost
          else
            edges(adjacency, node).foreach { (next, d) =>
              val nextCost = cost + d
              if next != start && best.get(next).forall(nextCost < _) then
                best(next) = nextCost
                queue.enqueue((nextCost, next))
            }
      found.toSet

    nodes.iterator.map(node => node -> edgesFrom(node)).toMap

  /** Dijkstra that records every equally cheap parent of each node, stops once the queue is past the
    * shortest distance to an end, then unwinds the parents into paths. Assumes positive weights.
    */
  def allShortestPaths[N, D: Numeric](
      adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
      start: N,
      ends: N => Boolean,
  ): Set[Path[N, D]] =
    val best = mutable.HashMap(start -> Numeric[D].zero)
    val parents = mutable.HashMap.empty[N, mutable.Set[N]]
    given Ordering[(D, N)] = Ordering.by[(D, N), D](_._1).reverse
    val queue = mutable.PriorityQueue((Numeric[D].zero, start))
    var shortest = Option.empty[D]
    val endsFound = mutable.Set.empty[N]

    while queue.nonEmpty && shortest.forall(queue.head._1 <= _) do
      val (cost, node) = queue.dequeue()
      if cost <= best(node) then
        if ends(node) then
          shortest = Some(cost)
          endsFound += node
        else
          edges(adjacency, node).foreach { (next, d) =>
            val nextCost = cost + d
            best.get(next) match
              case Some(known) if nextCost > known => ()
              case Some(known) if nextCost == known => parents(next) += node
              case _ =>
                best(next) = nextCost
                parents(next) = mutable.Set(node)
                queue.enqueue((nextCost, next))
          }

    def pathsTo(node: N): Iterator[Vector[N]] =
      if node == start then Iterator(Vector(start))
      else parents(node).iterator.flatMap(parent => pathsTo(parent).map(_ :+ node))

    shortest match
      case Some(distance) => endsFound.iterator.flatMap(pathsTo).map(Path(_, distance)).toSet
      case None => Set.empty

  /** Cycle detection by hashing states (or a key of them): calls `step` exactly offset + period times,
    * where brent's algorithm calls it roughly four times as often. It keeps the same history the
    * current version does, so it uses no more memory.
    */
  def findCycleBy[S, K](initial: S)(step: S => S, key: S => K): Cycle[S] =
    val seen = mutable.HashMap.empty[K, Int]
    val history = mutable.ArrayBuffer.empty[S]
    var state = initial
    var k = key(state)
    while !seen.contains(k) do
      seen(k) = history.size
      history += state
      state = step(state)
      k = key(state)
    val offset = seen(k)
    Cycle(offset, history.size - offset, history.toVector)

  def findCycle[S](initial: S)(step: S => S): Cycle[S] = findCycleBy(initial)(step, identity)

/** The same candidates written with only `val`s and immutable collections. */
object immutableCandidates:
  import scala.annotation.tailrec
  import scala.collection.immutable.TreeMap

  /** Iterates `start..end` (which must be non-empty), stopping on reaching `end` rather than
    * stepping past it, so it never computes `end + 1`.
    */
  def intervalIterator[N: Integral](start: N, end: N): Iterator[N] =
    Iterator.iterate(start)(_ + `1`).takeWhile(_ != end) ++ Iterator.single(end)

  def rectIterator[N: Integral](rect: Rect[N]): Iterator[Point[N]] =
    for
      y <- intervalIterator(rect.yRange.start, rect.yRange.end)
      x <- intervalIterator(rect.xRange.start, rect.xRange.end)
    yield Point(x, y)

  /** An immutable min priority queue, keeping elements of equal priority together in a bucket. */
  case class MinQueue[P: Ordering, A](buckets: TreeMap[P, List[A]]):
    def enqueue(priority: P, a: A): MinQueue[P, A] =
      MinQueue(buckets.updated(priority, a :: buckets.getOrElse(priority, Nil)))

    def dequeue: Option[(P, A, MinQueue[P, A])] =
      buckets.headOption.map((priority, bucket) =>
        val rest = if bucket.tail.isEmpty then buckets - priority else buckets.updated(priority, bucket.tail)
        (priority, bucket.head, MinQueue(rest))
      )

  object MinQueue:
    def apply[P: Ordering, A](priority: P, a: A): MinQueue[P, A] = MinQueue(TreeMap(priority -> List(a)))

  private def unwind[N](parent: Map[N, N], end: N): Vector[N] =
    (end :: List.unfold(end)(node => parent.get(node).map(p => (p, p)))).reverse.toVector

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
        // a node is queued again whenever a cheaper route to it is found, so skip the stale entries
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

  def compress[N, D: Numeric](adjacency: N => Iterable[(N, D)] | Iterator[(N, D)], nodes: Set[N]): WeightedGraph[N, D] =
    // dijkstra from `start` that records, but does not walk through, the other important nodes
    def edgesFrom(start: N): Set[(N, D)] =
      @tailrec
      def search(queue: MinQueue[D, N], best: Map[N, D], found: Map[N, D]): Set[(N, D)] =
        queue.dequeue match
          case None => found.toSet
          case Some((cost, node, rest)) if cost > best(node) => search(rest, best, found)
          case Some((cost, node, rest)) if node != start && nodes.contains(node) =>
            search(rest, best, found.updated(node, cost))
          case Some((cost, node, rest)) =>
            val (nextQueue, nextBest) = adjacency(node).iterator
              .filter((next, _) => next != start)
              .foldLeft((rest, best)) { case (acc @ (queue, best), (next, dist)) =>
                val nextCost = cost + dist
                if best.get(next).exists(_ <= nextCost) then acc
                else (queue.enqueue(nextCost, next), best.updated(next, nextCost))
              }
            search(nextQueue, nextBest, found)

      val zero = Numeric[D].zero
      search(MinQueue(zero, start), Map(start -> zero), Map.empty)

    nodes.iterator.map(node => node -> edgesFrom(node)).toMap

  def allShortestPaths[N, D: Numeric](
      adjacency: N => Iterable[(N, D)] | Iterator[(N, D)],
      start: N,
      ends: N => Boolean,
  ): Set[Path[N, D]] =
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

  def findCycleBy[S, K](initial: S)(step: S => S, key: S => K): Cycle[S] =
    @tailrec
    def loop(state: S, seen: Map[K, Int], history: Vector[S]): Cycle[S] =
      val k = key(state)
      seen.get(k) match
        case Some(offset) => Cycle(offset, history.size - offset, history)
        case None => loop(step(state), seen.updated(k, history.size), history :+ state)
    loop(initial, Map.empty, Vector.empty)

  def findCycle[S](initial: S)(step: S => S): Cycle[S] = findCycleBy(initial)(step, identity)
