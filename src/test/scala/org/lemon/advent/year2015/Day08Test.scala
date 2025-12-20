package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day08.*

class Day08Test extends UnitTest:

  test("part 1 example") {
    val in = """|""
                |"abc"
                |"aaa\"aaa"
                |"\x27"
                |""".stripMargin
    part1(in) shouldBe 12
  }

  test("part 1") {
    part1(read(file(2015)(8))) shouldBe 1342
  }

  test("part 2 example") {
    val in = """|""
                |"abc"
                |"aaa\"aaa"
                |"\x27"
                |""".stripMargin
    part2(in) shouldBe 19
  }

  test("part 2") {
    part2(read(file(2015)(8))) shouldBe 2074
  }
