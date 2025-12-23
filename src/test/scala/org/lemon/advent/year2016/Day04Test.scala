package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day04.*

class Day04Test extends UnitTest:

  test("part 1 example") {
    val in = """|aaaaa-bbb-z-y-x-123[abxyz]
                |a-b-c-d-e-f-g-h-987[abcde]
                |not-a-real-room-404[oarel]
                |totally-real-room-200[decoy]
                |""".stripMargin
    part1(in) shouldBe 123 + 987 + 404
  }

  test("part 1") {
    part1(read(file(2016)(4))) shouldBe 278221
  }

  test("part 2") {
    part2(read(file(2016)(4))) shouldBe 267
  }
