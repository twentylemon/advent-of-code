package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day15.*

class Day15Test extends UnitTest:

  test("part 1 example") {
    val in = """|Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
                |Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
                |""".stripMargin
    part1(in) shouldBe 62842880
  }

  test("part 1") {
    part1(read(file(2015)(15))) shouldBe 13882464
  }

  test("part 2 example") {
    val in = """|Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
                |Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3
                |""".stripMargin
    part2(in) shouldBe 57600000
  }

  test("part 2") {
    part2(read(file(2015)(15))) shouldBe 11171160
  }
