package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day21.*

class Day21Test extends UnitTest:

  test("part 1 example") {
    val in = """|swap position 4 with position 0
                |swap letter d with letter b
                |reverse positions 0 through 4
                |rotate left 1 step
                |move position 1 to position 4
                |move position 3 to position 0
                |rotate based on position of letter b
                |rotate based on position of letter d
                |""".stripMargin
    part1(in, seed = "abcde") shouldBe "decab"
  }

  test("part 1") {
    part1(read(file(2016)(21))) shouldBe "gbhafcde"
  }

  test("part 2") {
    part2(read(file(2016)(21))) shouldBe "bcfaegdh"
  }
