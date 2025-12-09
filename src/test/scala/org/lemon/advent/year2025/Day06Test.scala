package org.lemon.advent.year2025

import org.lemon.advent.*
import org.lemon.advent.year2025.Day06.*

class Day06Test extends UnitTest:

  test("part 1 example") {
    val in = """|123 328  51 64 
                | 45 64  387 23 
                |  6 98  215 314
                |*   +   *   +  
                |""".stripMargin
    part1(in) shouldBe 4277556
  }

  test("part 1") {
    part1(read(file(2025)(6))) shouldBe 3525371263915L
  }

  test("part 2 example") {
    val in = """|123 328  51 64 
                | 45 64  387 23 
                |  6 98  215 314
                |*   +   *   +  
                |""".stripMargin
    part2(in) shouldBe 3263827
  }

  test("part 2") {
    part2(read(file(2025)(6))) shouldBe 6846480843636L
  }
