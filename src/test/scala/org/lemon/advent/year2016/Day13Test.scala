package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day13.*

class Day13Test extends UnitTest:

  test("part 1 example") {
    val in = "10"
    part1(in, end = (7, 4)) shouldBe 11
  }

  test("part 1") {
    part1(read(file(2016)(13))) shouldBe 96
  }

  test("part 2") {
    part2(read(file(2016)(13))) shouldBe 141
  }
