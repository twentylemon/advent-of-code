package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day11.*

class Day11Test extends UnitTest:

  for
    (input, expected) <- Seq(
      "abcdefgh" -> "abcdffaa",
      "ghijklmn" -> "ghjaabcc"
    )
  do
    test(s"part 1 example $input") {
      part1(input) shouldBe expected
    }

  test("part 1") {
    part1(read(file(2015)(11))) shouldBe "hepxxyzz"
  }

  test("part 2") {
    part2(read(file(2015)(11))) shouldBe "heqaabcc"
  }
