package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day21.*

class Day21Test extends UnitTest:

  test("part 1 example") {
    val in = """|029A
                |980A
                |179A
                |456A
                |379A""".stripMargin
    part1(in) shouldBe 126384
  }
  

  test("part 1") {
    part1(read(file(2024)(21))) shouldBe 222670
  }

  test("part 2") {
    part2(read(file(2024)(21))) shouldBe 271397390297138L
  }
