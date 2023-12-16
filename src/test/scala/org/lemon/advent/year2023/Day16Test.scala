package org.lemon.advent.year2023

import org.lemon.advent._
import org.lemon.advent.year2023.Day16._

class Day16Test extends UnitTest:

  val in = """|.|...\....
              ||.-.\.....
              |.....|-...
              |........|.
              |..........
              |.........\
              |..../.\\..
              |.-.-/..|..
              |.|....-|.\
              |..//.|....""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 46
  }

  test("part 1") {
    part1(read(file(2023)(16))) shouldBe 7199
  }

  test("part 2 example") {
    part2(in) shouldBe 51
  }

  test("part 2") {
    part2(read(file(2023)(16))) shouldBe 7438
  }
