package org.lemon.advent.year2024

import org.lemon.advent._
import org.lemon.advent.year2024.Day22._

class Day22Test extends UnitTest:

  test("part 1 example") {
    val in = """|1
                |10
                |100
                |2024""".stripMargin
    part1(in) shouldBe 37327623L
  }

  test("part 1") {
    part1(read(file(2024)(22))) shouldBe 14623556510L
  }

  test("part 2 example") {
    val in = """|1
                |2
                |3
                |2024""".stripMargin
    part2(in) shouldBe 23
  }

  test("part 2") {
    part2(read(file(2024)(22))) shouldBe 1701
  }
