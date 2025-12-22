package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day20.*

class Day20Test extends UnitTest:

  for
    (input, expected) <- Seq(
      "120" -> 6,
      "130" -> 8
    )
  do
    test(s"part 1 example $input") {
      part1(input) shouldBe expected
    }

  test("part 1") {
    part1(read(file(2015)(20))) shouldBe 831600
  }

  test("part 2") {
    part2(read(file(2015)(20))) shouldBe 884520
  }
