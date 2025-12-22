package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day21.*

class Day21Test extends UnitTest:

  test("part 1") {
    part1(read(file(2015)(21))) shouldBe 78
  }

  test("part 2") {
    part2(read(file(2015)(21))) shouldBe 148
  }
