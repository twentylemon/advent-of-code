package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day01.*

class Day01Test extends UnitTest:

  for
    (input, expected) <- Seq(
      "R2, L3" -> 5,
      "R2, R2, R2" -> 2,
      "R5, L5, R5, R3" -> 12,
    )
  do
    test(s"part 1 example $input") {
      part1(input) shouldBe expected
    }

  test("part 1") {
    part1(read(file(2016)(1))) shouldBe 250
  }

  test("part 2 example") {
    val in = "R8, R4, R4, R8"
    part2(in) shouldBe 4
  }

  test("part 2") {
    part2(read(file(2016)(1))) shouldBe 151
  }
