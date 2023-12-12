package org.lemon.advent.lib

import org.scalacheck._
import org.lemon.advent._
import org.lemon.advent.lib.pairs

class CombinationsTest extends UnitTest:

  test("number of pairs equal to n choose 2") {
    check((xs: Seq[Int]) => xs.pairs.length == xs.size * (xs.size - 1) / 2)
  }

  test("elements from pairs are in collection") {
    check((xs: Seq[Int]) => xs.pairs.forall((a, b) => xs.contains(a) && xs.contains(b)))
  }
