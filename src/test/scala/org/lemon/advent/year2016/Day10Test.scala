package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day10.*

class Day10Test extends UnitTest:

  test("part 1 example") {
    val in = """|value 5 goes to bot 2
                |bot 2 gives low to bot 1 and high to bot 0
                |value 3 goes to bot 1
                |bot 1 gives low to output 1 and high to bot 0
                |bot 0 gives low to output 2 and high to output 0
                |value 2 goes to bot 2
                |""".stripMargin
    part1(in, compares = (2, 5)) shouldBe 2
  }

  test("part 1") {
    part1(read(file(2016)(10))) shouldBe 86
  }

  test("part 2 example") {
    val in = """|value 5 goes to bot 2
                |bot 2 gives low to bot 1 and high to bot 0
                |value 3 goes to bot 1
                |bot 1 gives low to output 1 and high to bot 0
                |bot 0 gives low to output 2 and high to output 0
                |value 2 goes to bot 2
                |""".stripMargin
    part2(in) shouldBe 5 * 2 * 3
  }

  test("part 2") {
    part2(read(file(2016)(10))) shouldBe 22847
  }
