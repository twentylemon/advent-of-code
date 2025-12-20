package org.lemon.advent.lib

import scala.collection.SeqOps
import scala.annotation.tailrec
import scala.util.NotGiven

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
    val indexed = it.toIndexedSeq
    for
      i <- indexed.indices.iterator
      j <- indexed.indices.drop(i + 1).iterator
    yield (indexed(i), indexed(j))

  /** Returns an iterator of all unordered triples of elements in the iterable.
    * If the collection contains duplicates, the triples will not be unique.
    *
    * @return iterator of all unordered triples
    */
  def triples: Iterator[(A, A, A)] =
    val indexed = it.toIndexedSeq
    for
      i <- indexed.indices.iterator
      j <- indexed.indices.drop(i + 1).iterator
      k <- indexed.indices.drop(j + 1).iterator
    yield (indexed(i), indexed(j), indexed(k))

  /** Returns a map of element frequencies.
    *
    * @return a map from each element to its count
    */
  def frequencies: Map[A, Int] = it.groupMapReduce(identity)(_ => 1)(_ + _)

  def slidingPairs: Iterable[(A, A)] = it.zip(it.tail)

extension [K, V](map: Map[K, V])
  /** Finds the first key associated with a given value.
    *
    * @param value the value to search for
    * @return the key associated with the value, or None if not found
    */
  def findValue(value: V): Option[K] = map.find(_._2 == value).map(_._1)

extension [A, CC[X] <: SeqOps[X, CC, CC[X]]](seq: CC[A])(using NotGiven[A =:= Char])
  /** Splits a collection by a given delimiting value. Behaves like `String#split`.
    *
    * @param delimiter the value to split the collection by
    * @return collection of each subcollection between delimiter values
    */
  def split(delimiter: A): CC[CC[A]] =
    val factory = seq.iterableFactory
    @tailrec
    def loop(remaining: CC[A], acc: Vector[CC[A]]): CC[CC[A]] =
      if remaining.isEmpty then
        if acc.isEmpty then factory.from(Vector(factory.empty))
        else factory.from(acc.take(acc.lastIndexWhere(_.nonEmpty) + 1))
      else
        val (before, after) = remaining.span(_ != delimiter)
        loop(after.drop(1), acc :+ before)
    loop(seq, Vector.empty)
