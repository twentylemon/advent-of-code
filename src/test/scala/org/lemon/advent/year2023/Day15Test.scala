package org.lemon.advent.year2023

import org.lemon.advent.*
import org.lemon.advent.year2023.Day15.*

class Day15Test extends UnitTest:

  val in = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

  test("part 1 example") {
    part1(in) shouldBe 1320
  }

  test("part 1") {
    part1(read(file(2023)(15))) shouldBe 514639
  }

  test("part 2 example") {
    part2(in) shouldBe 145
  }

  test("part 2") {
    part2(read(file(2023)(15))) shouldBe 279470
  }
