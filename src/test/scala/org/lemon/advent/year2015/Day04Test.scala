package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day04.*

class Day04Test extends UnitTest:

  for (input, expected) <- Seq(
    ("abcdef", 609043),
    ("pqrstuv", 1048970),
  ) do
    test(s"part 1 example $input") {
      part1(input) shouldBe expected
    }

  test("part 1") {
    part1(read(file(2015)(4))) shouldBe 117946
  }

  test("part 2") {
    part2(read(file(2015)(4))) shouldBe 3938038
  }
