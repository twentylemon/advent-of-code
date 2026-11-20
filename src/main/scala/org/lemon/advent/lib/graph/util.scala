package org.lemon.advent.lib.graph

import scala.collection.immutable.TreeMap

private def unitAdjacency[N](adjacency: N => Iterable[N] | Iterator[N]): N => Iterator[(N, Int)] =
  node => adjacency(node).iterator.map((_, 1))

/** An immutable min priority queue, keeping elements of equal priority together in a bucket. */
private enum MinQueue[P, A](using ord: Ordering[P]):
  case Empty[P: Ordering, A]() extends MinQueue[P, A]
  // the smallest bucket is never empty and is kept out of the tree, so most operations don't touch the tree
  case NonEmpty[P: Ordering, A](priority: P, bucket: List[A], rest: TreeMap[P, List[A]]) extends MinQueue[P, A]

  def enqueue(priority: P, a: A): MinQueue[P, A] = this match
    case Empty() => NonEmpty(priority, List(a), TreeMap.empty)
    case NonEmpty(smallest, bucket, rest) =>
      val order = ord.compare(priority, smallest)
      if order < 0 then NonEmpty(priority, List(a), rest.updated(smallest, bucket))
      else if order == 0 then NonEmpty(smallest, a :: bucket, rest)
      else NonEmpty(smallest, bucket, rest.updated(priority, a :: rest.getOrElse(priority, Nil)))

  def dequeue: Option[(P, A, MinQueue[P, A])] = this match
    case Empty() => None
    case NonEmpty(smallest, bucket, rest) =>
      val next: MinQueue[P, A] =
        if bucket.tail.nonEmpty then NonEmpty(smallest, bucket.tail, rest)
        else if rest.isEmpty then Empty()
        else
          val (nextPriority, nextBucket) = rest.head
          NonEmpty(nextPriority, nextBucket, rest.tail)
      Some((smallest, bucket.head, next))

private object MinQueue:
  def empty[P: Ordering, A]: MinQueue[P, A] = Empty()
  def apply[P: Ordering, A](priority: P, a: A): MinQueue[P, A] = NonEmpty(priority, List(a), TreeMap.empty)
