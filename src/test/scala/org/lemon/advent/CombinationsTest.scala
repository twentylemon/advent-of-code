package org.lemon.advent

import org.scalacheck._
import org.lemon.advent.pairs

class CombinationsTest extends UnitTest:

  test("number of pairs equal to n choose 2", Lib) {
    check((xs: Seq[Int]) => xs.pairs.length == xs.size * (xs.size - 1) / 2)
  }

  test("elements from pairs are in collection", Lib) {
    check((xs: Seq[Int]) => xs.pairs.forall((a, b) => xs.contains(a) && xs.contains(b)))
  }
