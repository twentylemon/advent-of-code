package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day05.*

class Day05Test extends UnitTest:

  test("part 1 example") {
    val in = "abc"
    part1(in) shouldBe "18f47a30"
  }

  test("part 1") {
    part1(read(file(2016)(5))) shouldBe "d4cd2ee1"
  }

  test("part 2 example") {
    val in = "abc"
    part2(in) shouldBe "05ace8e3"
  }

  test("part 2") {
    part2(read(file(2016)(5))) shouldBe "f2c730e5"
  }
