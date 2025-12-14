package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day20.*

class Day20Test extends UnitTest:

  test("mix example") {
    val in = """|1
                |2
                |-3
                |3
                |-2
                |0
                |4""".stripMargin
    mix(in.linesIterator.toSeq.map(_.toInt)) shouldBe Seq(1, 2, -3, 4, 0, 3, -2)
  }

  test("part 1 example") {
    val in = """|1
                |2
                |-3
                |3
                |-2
                |0
                |4""".stripMargin
    part1(in) shouldBe 4 + -3 + 2
  }

  test("part 1") {
    part1(read(file(2022)(20))) shouldBe 13183
  }

  test("part 2 example") {
    val in = """|1
                |2
                |-3
                |3
                |-2
                |0
                |4""".stripMargin
    part2(in) shouldBe 811589153L + 2434767459L + -1623178306L
  }

  test("part 2") {
    part2(read(file(2022)(20))) shouldBe 6676132372578L
  }
