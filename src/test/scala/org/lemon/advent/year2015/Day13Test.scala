package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day13.*

class Day13Test extends UnitTest:

  test("part 1 example") {
    val in = """|Alice would gain 54 happiness units by sitting next to Bob.
                |Alice would lose 79 happiness units by sitting next to Carol.
                |Alice would lose 2 happiness units by sitting next to David.
                |Bob would gain 83 happiness units by sitting next to Alice.
                |Bob would lose 7 happiness units by sitting next to Carol.
                |Bob would lose 63 happiness units by sitting next to David.
                |Carol would lose 62 happiness units by sitting next to Alice.
                |Carol would gain 60 happiness units by sitting next to Bob.
                |Carol would gain 55 happiness units by sitting next to David.
                |David would gain 46 happiness units by sitting next to Alice.
                |David would lose 7 happiness units by sitting next to Bob.
                |David would gain 41 happiness units by sitting next to Carol.
                |""".stripMargin
    part1(in) shouldBe 330
  }

  test("part 1") {
    part1(read(file(2015)(13))) shouldBe 709
  }

  test("part 2") {
    part2(read(file(2015)(13))) shouldBe 668
  }
