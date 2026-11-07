package org.lemon.advent.lib.graph

import scala.annotation.tailrec

object DisjointSet:
  /** Creates a disjoint set with each element in a group of its own.
    *
    * @param elements the elements of the set
    * @return a disjoint set of single element groups
    */
  def apply[A](elements: Iterable[A]): DisjointSet[A] =
    new DisjointSet(elements.iterator.map(a => a -> a).toMap, Map.empty)

/** A partition of elements into disjoint groups, which `union` merges. */
final class DisjointSet[A] private (parent: Map[A, A], rank: Map[A, Int]):
  /** Merges the groups containing two elements.
    *
    * @param a an element of the set
    * @param b an element of the set
    * @return a new disjoint set with a and b in the same group
    * @throws NoSuchElementException if a or b is not in the set
    */
  def union(a: A, b: A): DisjointSet[A] =
    val rootA = find(a)
    val rootB = find(b)
    if rootA == rootB then this
    else
      val rankA = rank.getOrElse(rootA, 0)
      val rankB = rank.getOrElse(rootB, 0)
      if rankA < rankB then new DisjointSet(parent.updated(rootA, rootB), rank)
      else if rankA > rankB then new DisjointSet(parent.updated(rootB, rootA), rank)
      else new DisjointSet(parent.updated(rootB, rootA), rank.updated(rootA, rankA + 1))

  /** Returns whether two elements are in the same group.
    *
    * @param a an element of the set
    * @param b an element of the set
    * @return true if a and b are in the same group
    * @throws NoSuchElementException if a or b is not in the set
    */
  def connected(a: A, b: A): Boolean = find(a) == find(b)

  /** @return an iterator over the groups of elements
    */
  def components: Iterator[Set[A]] = parent.keySet.groupBy(find).valuesIterator

  @tailrec private def find(a: A): A =
    val p = parent(a)
    if p == a then a else find(p)
