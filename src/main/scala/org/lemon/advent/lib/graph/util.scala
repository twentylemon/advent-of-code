package org.lemon.advent.lib.graph

import scala.collection.immutable.TreeMap

private def unitAdjacency[N](adjacency: N => Iterable[N] | Iterator[N]): N => Iterator[(N, Int)] =
  node => adjacency(node).iterator.map((_, 1))

/** An immutable min priority queue, keeping elements of equal priority together in a bucket. */
private case class MinQueue[P: Ordering, A](buckets: TreeMap[P, List[A]]):
  def enqueue(priority: P, a: A): MinQueue[P, A] =
    MinQueue(buckets.updated(priority, a :: buckets.getOrElse(priority, Nil)))

  def dequeue: Option[(P, A, MinQueue[P, A])] =
    buckets.headOption.map((priority, bucket) =>
      val rest = if bucket.tail.isEmpty then buckets - priority else buckets.updated(priority, bucket.tail)
      (priority, bucket.head, MinQueue(rest))
    )

private object MinQueue:
  def apply[P: Ordering, A](priority: P, a: A): MinQueue[P, A] = MinQueue(TreeMap(priority -> List(a)))
