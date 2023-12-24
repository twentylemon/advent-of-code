package org.lemon.advent.lib.graph

import scala.collection.mutable

type WeightedGraph[K, V] = Map[K, Seq[(K, V)]]

def pathFind[K, V](graph: WeightedGraph[K, V], start: K, end: K)(using numeric: Numeric[V]): V =
  val queue = mutable.Queue((Set(start), start, numeric.zero))
  while !queue.isEmpty && queue.head._2 != end do
    val (path, node, distance) = queue.dequeue
    queue ++= graph(node)
      .filterNot((neigh, _) => path.contains(neigh))
      .map((node, dist) => (path + node, node, numeric.plus(dist, distance)))

  queue.head._3

type UnitGraph[K] = Map[K, Seq[K]]

def pathFind[K](graph: UnitGraph[K], start: K, end: K): Int =
  val weighted = graph.map((k, n) => (k, n.map((_, 1))))
  pathFind(weighted, start, end)
