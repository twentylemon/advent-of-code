package org.lemon.advent

extension [T](xs: Iterable[T])

  def pairs: Iterator[(T, T)] =
    for
      (x, i) <- xs.iterator.zipWithIndex
      (y, j) <- xs.iterator.zipWithIndex
      if i < j
    yield (x, y)
