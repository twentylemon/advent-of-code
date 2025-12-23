package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day03.*

class Day03Test extends UnitTest:

  test("part 1 example") {
    val in = "5 10 25"
    part1(in) shouldBe 0
  }

  test("part 1") {
    part1(read(file(2016)(3))) shouldBe 982
  }

  test("part 2") {
    part2(read(file(2016)(3))) shouldBe 1826
  }
