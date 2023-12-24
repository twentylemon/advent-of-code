package org.lemon.advent.year2023

import org.lemon.advent._
import org.lemon.advent.year2023.Day24._

class Day24Test extends UnitTest:

  val in = """|19, 13, 30 @ -2,  1, -2
              |18, 19, 22 @ -1, -1, -2
              |20, 25, 34 @ -2, -2, -4
              |12, 31, 28 @ -1, -2, -1
              |20, 19, 15 @  1, -5, -3
              |""".stripMargin

  test("part 1 example") {
    part1(in, 7, 27) shouldBe 2
  }

  test("part 1") {
    part1(read(file(2023)(24))) shouldBe 31921
  }

  test("part 2 example") {
    part2(in, 2) shouldBe 24 + 13 + 10
  }

  test("part 2") {
    part2(read(file(2023)(24))) shouldBe 331109811422259L + 312547020340291L + 118035075297081L
  }
