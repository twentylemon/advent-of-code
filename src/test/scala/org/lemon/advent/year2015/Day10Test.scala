package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day10.*

class Day10Test extends UnitTest:

  test("part 1 example") {
    val in = "1"
    part1(in, times = 5) shouldBe "312211".size
  }

  test("part 1") {
    part1(read(file(2015)(10))) shouldBe 252594
  }

  test("part 2") {
    part2(read(file(2015)(10))) shouldBe 3579328
  }
