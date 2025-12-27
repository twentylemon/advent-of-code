package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day19.*

class Day19Test extends UnitTest:

  test("part 1 example") {
    val in = "5"
    part1(in) shouldBe 3
  }

  test("part 1") {
    part1(read(file(2016)(19))) shouldBe 1816277
  }

  test("part 2 example") {
    val in = "5"
    part2(in) shouldBe 2
  }

  test("part 2") {
    part2(read(file(2016)(19))) shouldBe 1410967
  }
