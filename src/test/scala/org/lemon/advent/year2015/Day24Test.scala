package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day24.*

class Day24Test extends UnitTest:

  test("part 1 example") {
    val in = """|1
                |2
                |3
                |4
                |5
                |7
                |8
                |9
                |10
                |11
                |""".stripMargin
    part1(in) shouldBe 99
  }

  test("part 1") {
    part1(read(file(2015)(24))) shouldBe 11266889531L
  }

  test("part 2 example") {
    val in = """|1
                |2
                |3
                |4
                |5
                |7
                |8
                |9
                |10
                |11
                |""".stripMargin
    part2(in) shouldBe 44
  }

  test("part 2") {
    part2(read(file(2015)(24))) shouldBe 77387711
  }
