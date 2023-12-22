package org.lemon.advent.year2023

import org.lemon.advent._
import org.lemon.advent.year2023.Day22._

class Day22Test extends UnitTest:

  val in = """|1,0,1~1,2,1
              |0,0,2~2,0,2
              |0,2,3~2,2,3
              |0,0,4~0,2,4
              |2,0,5~2,2,5
              |0,1,6~2,1,6
              |1,1,8~1,1,9
              |""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 5
  }

  test("part 1") {
    part1(read(file(2023)(22))) shouldBe 405
  }

  test("part 2 example") {
    part2(in) shouldBe 7
  }

  test("part 2") {
    part2(read(file(2023)(22))) shouldBe 61297
  }
