package org.lemon.advent.lib.graph

import scala.annotation.tailrec
import scala.collection.AbstractIterable

object DisjointSet:
  /** Creates a disjoint set with each element in a component of its own.
    *
    * @param elements the elements of the set
    * @return a disjoint set of single element components
    */
  def apply[A](elements: Iterable[A]): DisjointSet[A] =
    val parent = elements.iterator.map(a => a -> a).toMap
    new DisjointSet(parent, Map.empty, parent.size)

/** A partition of elements into disjoint components, which `union` merges. */
final class DisjointSet[A] private (parent: Map[A, A], rank: Map[A, Int], componentCount: Int):
  /** Merges the components containing two elements.
    *
    * @param a an element of the set
    * @param b an element of the set
    * @return a new disjoint set with a and b in the same component
    * @throws NoSuchElementException if a or b is not in the set
    */
  def union(a: A, b: A): DisjointSet[A] =
    val rootA = find(a)
    val rootB = find(b)
    if rootA == rootB then this
    else
      val rankA = rank.getOrElse(rootA, 0)
      val rankB = rank.getOrElse(rootB, 0)
      if rankA < rankB then new DisjointSet(parent.updated(rootA, rootB), rank, componentCount - 1)
      else if rankA > rankB then new DisjointSet(parent.updated(rootB, rootA), rank, componentCount - 1)
      else new DisjointSet(parent.updated(rootB, rootA), rank.updated(rootA, rankA + 1), componentCount - 1)

  /** Returns whether two elements are in the same component.
    *
    * @param a an element of the set
    * @param b an element of the set
    * @return true if a and b are in the same component
    * @throws NoSuchElementException if a or b is not in the set
    */
  def connected(a: A, b: A): Boolean = find(a) == find(b)

  /** @return the components, regrouped on each traversal, with a constant time `size` */
  def components: Iterable[Set[A]] = new AbstractIterable[Set[A]]:
    def iterator = parent.keySet.groupBy(DisjointSet.this.find).valuesIterator
    override def knownSize = componentCount

  @tailrec
  private def find(a: A): A =
    val p = parent(a)
    if p == a then a else find(p)
