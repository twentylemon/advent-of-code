package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day16.*

class Day16Test extends UnitTest:

  test("part 1 example") {
    part1("10000", length = 20) shouldBe "01100"
  }

  test("part 1") {
    part1(read(file(2016)(16))) shouldBe "10101001010100001"
  }

  test("part 2") {
    part2(read(file(2016)(16))) shouldBe "10100001110101001"
  }
