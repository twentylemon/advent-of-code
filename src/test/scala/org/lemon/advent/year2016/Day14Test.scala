package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day14.*

class Day14Test extends UnitTest:

  test("part 1 example") {
    val in = "abc"
    part1(in) shouldBe 22728
  }

  test("part 1") {
    part1(read(file(2016)(14))) shouldBe 23769
  }

  test("part 2 example") {
    val in = "abc"
    part2(in) shouldBe 22551
  }

  test("part 2") {
    part2(read(file(2016)(14))) shouldBe 20606
  }
