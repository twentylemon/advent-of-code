package org.lemon.advent.year2024

import org.lemon.advent.*
import org.lemon.advent.year2024.Day07.*

class Day07Test extends UnitTest:

  test("part 1 example") {
    val in = """|190: 10 19
                |3267: 81 40 27
                |83: 17 5
                |156: 15 6
                |7290: 6 8 6 15
                |161011: 16 10 13
                |192: 17 8 14
                |21037: 9 7 18 13
                |292: 11 6 16 20""".stripMargin
    part1(in) shouldBe 3749
  }

  test("part 1") {
    part1(read(file(2024)(7))) shouldBe 5030892084481L
  }

  test("part 2 example") {
    val in = """|190: 10 19
                |3267: 81 40 27
                |83: 17 5
                |156: 15 6
                |7290: 6 8 6 15
                |161011: 16 10 13
                |192: 17 8 14
                |21037: 9 7 18 13
                |292: 11 6 16 20""".stripMargin
    part2(in) shouldBe 11387
  }

  test("part 2") {
    part2(read(file(2024)(7))) shouldBe 91377448644679L
  }
