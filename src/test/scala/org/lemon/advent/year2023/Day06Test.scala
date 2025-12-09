package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day06.*

class Day06Test extends UnitTest:
  
  val in = """|Time:      7  15   30
              |Distance:  9  40  200""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 288
  }

  test("part 1") {
    part1(read(file(2023)(6))) shouldBe 219849
  }

  test("part 2 example") {
    part2(in) shouldBe 71503
  }

  test("part 2") {
    part2(read(file(2023)(6))) shouldBe 29432455L
  }
