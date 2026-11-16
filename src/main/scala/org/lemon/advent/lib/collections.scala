package org.lemon.advent.lib

import scala.collection.{AbstractIterator, LazyZip2, LazyZip3, LazyZip4, SeqOps}
import scala.util.NotGiven

extension [A](it: Iterator[A])
  /** Returns the nth element of the iterator.
    *
    * @param n the index of the element to return
    * @throws IndexOutOfBoundsException if n is negative
    * @throws NoSuchElementException if there is no nth element
    * @return the nth element of the iterator
    */
  def nth(n: Int): A =
    if n < 0 then throw IndexOutOfBoundsException(n.toString)
    it.drop(n).next()

  /** Returns the nth element of the iterator, or None if there is no nth element.
    *
    * @param n the index of the element to return
    * @return the nth element of the iterator, or None if there is no nth element
    */
  def nthOption(n: Int): Option[A] = if n < 0 then None else it.drop(n).nextOption()

  /** Returns the last element of the iterator.
    *
    * @return the last element
    */
  def last: A = it.reduceLeft((_, b) => b)

  /** Returns `sliding(2)` as tuples instead of collections.
    * Iterators with fewer than 2 elements yield an empty result.
    *
    * @return sliding window of pairs in the iterator
    */
  def sliding2: Iterator[(A, A)] =
    val windows =
      for a <- it.nextOption()
      yield new AbstractIterator[(A, A)]:
        private var first = a
        def hasNext: Boolean = it.hasNext
        def next(): (A, A) =
          val window = (first, it.next())
          first = window._2
          window
    windows.getOrElse(Iterator.empty)

  /** Returns `sliding(3)` as tuples instead of collections.
    * Iterators with fewer than 3 elements yield an empty result.
    *
    * @return sliding window of triples in the iterator
    */
  def sliding3: Iterator[(A, A, A)] =
    val windows =
      for a <- it.nextOption(); b <- it.nextOption()
      yield new AbstractIterator[(A, A, A)]:
        private var first = a
        private var second = b
        def hasNext: Boolean = it.hasNext
        def next(): (A, A, A) =
          val window = (first, second, it.next())
          first = second
          second = window._3
          window
    windows.getOrElse(Iterator.empty)

  /** Returns `sliding(4)` as tuples instead of collections.
    * Iterators with fewer than 4 elements yield an empty result.
    *
    * @return sliding window of 4-tuples in the iterator
    */
  def sliding4: Iterator[(A, A, A, A)] =
    val windows =
      for a <- it.nextOption(); b <- it.nextOption(); c <- it.nextOption()
      yield new AbstractIterator[(A, A, A, A)]:
        private var first = a
        private var second = b
        private var third = c
        def hasNext: Boolean = it.hasNext
        def next(): (A, A, A, A) =
          val window = (first, second, third, it.next())
          first = second
          second = third
          third = window._4
          window
    windows.getOrElse(Iterator.empty)

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
      case a :: rest <- it.toList.tails
      b <- rest.iterator
    yield (a, b)

  /** Returns an iterator of all unordered triples of elements in the iterable.
    * If the collection contains duplicates, the triples will not be unique.
    *
    * @return iterator of all unordered triples
    */
  def triples: Iterator[(A, A, A)] =
    for
      case a :: rest <- it.toList.tails
      case b :: rest2 <- rest.tails
      c <- rest2.iterator
    yield (a, b, c)

  /** Returns `sliding(2)` as tuples instead of collections.
    * Collections with fewer than 2 elements yield an empty result.
    *
    * @return sliding window of pairs in the collection
    */
  def sliding2: LazyZip2[A, A, it.type] = it lazyZip it.drop(1)

  /** Returns `sliding(3)` as tuples instead of collections.
    * Collections with fewer than 3 elements yield an empty result.
    *
    * @return sliding window of triples in the collection
    */
  def sliding3: LazyZip3[A, A, A, it.type] = it lazyZip it.drop(1) lazyZip it.drop(2)

  /** Returns `sliding(4)` as tuples instead of collections.
    * Collections with fewer than 4 elements yield an empty result.
    *
    * @return sliding window of 4-tuples in the collection
    */
  def sliding4: LazyZip4[A, A, A, A, it.type] = it lazyZip it.drop(1) lazyZip it.drop(2) lazyZip it.drop(3)

  /** Returns a map of element frequencies.
    *
    * @return a map from each element to its count
    */
  def frequencies: Map[A, Int] = it.groupMapReduce(identity)(_ => 1)(_ + _)

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
    if seq.isEmpty then factory.from(Vector(factory.empty))
    else
      val parts = Vector.newBuilder[CC[A]]
      var part = factory.newBuilder[A]
      seq.foreach(value =>
        if value == delimiter then
          parts += part.result()
          part = factory.newBuilder[A]
        else part += value
      )
      parts += part.result()
      val all = parts.result()
      factory.from(all.take(all.lastIndexWhere(_.nonEmpty) + 1))
