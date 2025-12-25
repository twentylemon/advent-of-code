package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day11.*

class Day11Test extends UnitTest:

  test("part 1 example") {
    val in = """|The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
                |The second floor contains a hydrogen generator.
                |The third floor contains a lithium generator.
                |The fourth floor contains nothing relevant.
                |""".stripMargin
    part1(in) shouldBe 11
  }

  test("part 1") {
    part1(read(file(2016)(11))) shouldBe 47
  }

  test("part 2") {
    part2(read(file(2016)(11))) shouldBe 71
  }
