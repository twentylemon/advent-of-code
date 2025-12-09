package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day09.*

class Day09Test extends UnitTest:

  val in = """|0 3 6 9 12 15
              |1 3 6 10 15 21
              |10 13 16 21 30 45""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 114
  }

  test("part 1") {
    part1(read(file(2023)(9))) shouldBe 1853145119
  }

  test("part 2 example") {
    part2(in) shouldBe 2
  }

  test("part 2") {
    part2(read(file(2023)(9))) shouldBe 923
  }
