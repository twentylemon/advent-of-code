package org.lemon.advent.lib

import org.scalacheck._
import org.lemon.advent._
import org.lemon.advent.lib.{pairs, triples}

class CombinationsTest extends UnitTest:

  test("pairs is equal to stdlib combinations(2) of a set") {
    check((xs: Set[Int]) =>
      val pair = xs.pairs.toSeq.sorted
      val combo = xs.toSeq
        .combinations(2)
        .map { case Seq(a, b) => (a, b) }
        .toSeq
        .sorted
      pair == combo
    )
  }

  test("triples is equal to stdlib combinations(3) of a set") {
    check((xs: Set[Int]) =>
      val trip = xs.triples.toSeq.sorted
      val combo = xs.toSeq
        .combinations(3)
        .map { case Seq(a, b, c) => (a, b, c) }
        .toSeq
        .sorted
      trip == combo
    )
  }
