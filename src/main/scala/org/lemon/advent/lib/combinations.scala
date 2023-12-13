package org.lemon.advent.lib

extension [T](xs: Iterable[T])

  def pairs: Iterator[(T, T)] =
    for
      (x, i) <- xs.iterator.zipWithIndex
      (y, j) <- xs.iterator.zipWithIndex
      if i < j
    yield (x, y)

  def triples: Iterator[(T, T, T)] =
    for
      (x, i) <- xs.iterator.zipWithIndex
      (y, j) <- xs.iterator.zipWithIndex
      (z, k) <- xs.iterator.zipWithIndex
      if i < j
      if j < k
    yield (x, y, z)
