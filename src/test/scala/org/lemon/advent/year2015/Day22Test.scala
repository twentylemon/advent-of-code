package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day22.*

class Day22Test extends UnitTest:

  test("part 1") {
    part1(read(file(2015)(22))) shouldBe 900
  }

  test("part 2") {
    part2(read(file(2015)(22))) shouldBe 1216
  }
