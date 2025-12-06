package org.lemon.advent.lib

import scala.collection.SeqOps
import scala.annotation.tailrec

extension [A](it: Iterator[A])
  /** Returns the nth element of the iterator.
    *
    * @param n the index of the element to return
    * @throws NoSuchElementException if there is no nth element
    * @return the nth element of the iterator
    */
  def nth(n: Int): A = it.drop(n).next

  /** Returns the nth element of the iterator, or None if there is no nth element.
    *
    * @param n the index of the element to return
    * @return the nth element of the iterator, or None if there is no nth element
    */
  def nthOption(n: Int): Option[A] = it.drop(n).nextOption

extension [A](it: Iterable[A])
  /** Returns an iterator view of the Cartesian product of this iterable and another.
    * Also known as the cross product.
    *
    * @param rhs the other iterable
    * @return all pairs of elements (a, b) where a is from this iterable and b is from the other iterable
    */
  def cartesianProduct[B](rhs: Iterable[B]): Iterator[(A, B)] =
    for a <- it.iterator; b <- rhs.iterator yield (a, b)

  /** Returns an iterator of all unordered pairs of elements in the iterable.
    * If the collection contains duplicates, the pairs will not be unique.
    *
    * @return iterator of all unordered pairs
    */
  def pairs: Iterator[(A, A)] =
    for
      (x, i) <- it.iterator.zipWithIndex
      (y, j) <- it.iterator.zipWithIndex
      if i < j
    yield (x, y)

  /** Returns an iterator of all unordered triples of elements in the iterable.
    * If the collection contains duplicates, the triples will not be unique.
    *
    * @return iterator of all unordered triples
    */
  def triples: Iterator[(A, A, A)] =
    for
      (x, i) <- it.iterator.zipWithIndex
      (y, j) <- it.iterator.zipWithIndex
      (z, k) <- it.iterator.zipWithIndex
      if i < j
      if j < k
    yield (x, y, z)

extension [A, CC[X] <: SeqOps[X, CC, CC[X]]](seq: CC[A])
  /** Splits a collection by a given delimiting value. Behaves like `String#split`.
    *
    * @param delimiter the value to split the collection by
    * @return collection of each subcollection between delimiter values
    */
  def split(delimiter: A): CC[CC[A]] =
    val factory = seq.iterableFactory
    @tailrec
    def loop(remaining: CC[A], acc: Vector[CC[A]]): CC[CC[A]] =
      if remaining.isEmpty then factory.from(acc)
      else
        val (before, after) = remaining.span(_ != delimiter)
        loop(after.drop(1), if before.nonEmpty then acc :+ before else acc)
    loop(seq, Vector.empty)
