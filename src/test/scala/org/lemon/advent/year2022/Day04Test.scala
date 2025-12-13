package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day04.*

class Day04Test extends UnitTest:

  test("part 1 example") {
    val in = """|2-4,6-8
                |2-3,4-5
                |5-7,7-9
                |2-8,3-7
                |6-6,4-6
                |2-6,4-8""".stripMargin
    part1(in) shouldBe 2
  }

  test("part 1") {
    part1(read(file(2022)(4))) shouldBe 485
  }

  test("part 2 example") {
    val in = """|2-4,6-8
                |2-3,4-5
                |5-7,7-9
                |2-8,3-7
                |6-6,4-6
                |2-6,4-8""".stripMargin
    part2(in) shouldBe 4
  }

  test("part 2") {
    part2(read(file(2022)(4))) shouldBe 857
  }
