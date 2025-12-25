package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day12.*

class Day12Test extends UnitTest:

  test("part 1 example") {
    val in = """|cpy 41 a
                |inc a
                |inc a
                |dec a
                |jnz a 2
                |dec a
                |""".stripMargin
    part1(in) shouldBe 42
  }

  test("part 1") {
    part1(read(file(2016)(12))) shouldBe 318117
  }

  test("part 2") {
    part2(read(file(2016)(12))) shouldBe 9227771
  }
