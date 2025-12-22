package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day23.*

class Day23Test extends UnitTest:

  test("part 1 example") {
    val in = """|inc a
                |jio a, +2
                |tpl a
                |inc a
                |""".stripMargin
    part1(in, register = "a") shouldBe 2
  }

  test("part 1") {
    part1(read(file(2015)(23))) shouldBe 255
  }

  test("part 2") {
    part2(read(file(2015)(23))) shouldBe 334
  }
