package org.lemon.advent.lib

extension [T](xs: Iterable[T])
  /** Returns an iterator of all unordered pairs of elements in the iterable.
    * If the collection contains duplicates, the pairs will not be unique.
    *
    * @return iterator of all unordered pairs
    */
  def pairs: Iterator[(T, T)] =
    for
      (x, i) <- xs.iterator.zipWithIndex
      (y, j) <- xs.iterator.zipWithIndex
      if i < j
    yield (x, y)

  /** Returns an iterator of all unordered triples of elements in the iterable.
    * If the collection contains duplicates, the triples will not be unique.
    *
    * @return iterator of all unordered triples
    */
  def triples: Iterator[(T, T, T)] =
    for
      (x, i) <- xs.iterator.zipWithIndex
      (y, j) <- xs.iterator.zipWithIndex
      (z, k) <- xs.iterator.zipWithIndex
      if i < j
      if j < k
    yield (x, y, z)
