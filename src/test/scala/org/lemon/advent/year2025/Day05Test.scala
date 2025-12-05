package org.lemon.advent.year2025

import org.lemon.advent._
import org.lemon.advent.year2025.Day05._

class Day05Test extends UnitTest:

  test("part 1 example") {
    val in = """|3-5
                |10-14
                |16-20
                |12-18
                |
                |1
                |5
                |8
                |11
                |17
                |32
                |""".stripMargin
    part1(in) shouldBe 3
  }

  test("part 1") {
    part1(read(file(2025)(5))) shouldBe 726
  }

  test("part 2 example") {
    val in = """|3-5
                |10-14
                |16-20
                |12-18
                |
                |1
                |5
                |8
                |11
                |17
                |32
                |""".stripMargin
    part2(in) shouldBe 14
  }

  test("part 2") {
    part2(read(file(2025)(5))) shouldBe 354226555270043L
  }
