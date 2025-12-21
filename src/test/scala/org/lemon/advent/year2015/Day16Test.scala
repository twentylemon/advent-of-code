package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day16.*

class Day16Test extends UnitTest:

  test("part 1") {
    part1(read(file(2015)(16))) shouldBe 213
  }

  test("part 2") {
    part2(read(file(2015)(16))) shouldBe 323
  }
