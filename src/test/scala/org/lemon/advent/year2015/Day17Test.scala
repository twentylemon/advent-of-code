package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day17.*

class Day17Test extends UnitTest:

  test("part 1 example") {
    val in = """|20
                |15
                |10
                |5
                |5
                |""".stripMargin
    part1(in, target = 25) shouldBe 4
  }

  test("part 1") {
    part1(read(file(2015)(17))) shouldBe 1638
  }

  test("part 2 example") {
    val in = """|20
                |15
                |10
                |5
                |5
                |""".stripMargin
    part2(in, target = 25) shouldBe 3
  }

  test("part 2") {
    part2(read(file(2015)(17))) shouldBe 17
  }
