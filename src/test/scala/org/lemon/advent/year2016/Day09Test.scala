package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day09.*

class Day09Test extends UnitTest:

  for
    (input, expected) <- Seq(
      "ADVENT" -> 6,
      "A(1x5)BC" -> 7,
      "(3x3)XYZ" -> 9,
      "A(2x2)BCD(2x2)EFG" -> 11,
      "(6x1)(1x3)A" -> 6,
      "X(8x2)(3x3)ABCY" -> 18,
    )
  do
    test(s"part 1 example $input") {
      part1(input) shouldBe expected
    }

  test("part 1") {
    part1(read(file(2016)(9))) shouldBe 183269
  }

  for
    (input, expected) <- Seq(
      "(3x3)XYZ" -> 9,
      "X(8x2)(3x3)ABCY" -> "XABCABCABCABCABCABCY".size,
      "(27x12)(20x12)(13x14)(7x10)(1x12)A" -> 241920,
      "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" -> 445,
    )
  do
    test(s"part 2 example $input") {
      part2(input) shouldBe expected
    }

  test("part 2") {
    part2(read(file(2016)(9))) shouldBe 11317278863L
  }
