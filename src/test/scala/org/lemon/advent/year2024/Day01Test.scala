package org.lemon.advent.year2024

import org.lemon.advent._
import org.lemon.advent.year2024.Day01._

class Day01Test extends UnitTest:

  test("part 1 example") {
    val in = """|3   4
                |4   3
                |2   5
                |1   3
                |3   9
                |3   3""".stripMargin
    part1(in) shouldBe 11
  }

  test("part 1") {
    part1(read(file(2024)(1))) shouldBe 1341714
  }

  test("part 2 example") {
    val in = """|3   4
                |4   3
                |2   5
                |1   3
                |3   9
                |3   3""".stripMargin
    part2(in) shouldBe 31
  }

  test("part 2") {
    part2(read(file(2024)(1))) shouldBe 27384707
  }
