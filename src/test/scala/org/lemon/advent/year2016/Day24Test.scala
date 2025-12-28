package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day24.*

class Day24Test extends UnitTest:

  test("part 1 example") {
    val in = """|###########
                |#0.1.....2#
                |#.#######.#
                |#4.......3#
                |###########
                |""".stripMargin
    part1(in) shouldBe 14
  }

  test("part 1") {
    part1(read(file(2016)(24))) shouldBe 498
  }

  test("part 2") {
    part2(read(file(2016)(24))) shouldBe 804
  }
