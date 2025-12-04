package org.lemon.advent.year2025

import org.lemon.advent._
import org.lemon.advent.year2025.Day04._

class Day04Test extends UnitTest:

  test("part 1 example") {
    val in = """|..@@.@@@@.
                |@@@.@.@.@@
                |@@@@@.@.@@
                |@.@@@@..@.
                |@@.@@@@.@@
                |.@@@@@@@.@
                |.@.@.@.@@@
                |@.@@@.@@@@
                |.@@@@@@@@.
                |@.@.@@@.@.
                |""".stripMargin
    part1(in) shouldBe 13
  }

  test("part 1") {
    part1(read(file(2025)(4))) shouldBe 1409
  }

  test("part 2 example") {
    val in = """|..@@.@@@@.
                |@@@.@.@.@@
                |@@@@@.@.@@
                |@.@@@@..@.
                |@@.@@@@.@@
                |.@@@@@@@.@
                |.@.@.@.@@@
                |@.@@@.@@@@
                |.@@@@@@@@.
                |@.@.@@@.@.
                |""".stripMargin
    part2(in) shouldBe 43
  }

  test("part 2") {
    part2(read(file(2025)(4))) shouldBe 8366
  }
