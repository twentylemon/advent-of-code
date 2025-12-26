package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day18.*

class Day18Test extends UnitTest:

  test("part 1 example") {
    val in = ".^^.^.^^^^"
    part1(in, rows = 10) shouldBe 38
  }

  test("part 1") {
    part1(read(file(2016)(18))) shouldBe 1913
  }

  test("part 2") {
    part2(read(file(2016)(18))) shouldBe 19993564
  }
