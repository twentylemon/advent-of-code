package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day18.*

class Day18Test extends UnitTest:

  test("part 1 combined cubes") {
    val in = """|1,1,1
                |2,1,1""".stripMargin
    part1(in) shouldBe 10
  }

  test("part 1 separate cubes") {
    val in = """|1,1,1
                |3,1,1""".stripMargin
    part1(in) shouldBe 12
  }

  test("part 1 example") {
    val in = """|2,2,2
                |1,2,2
                |3,2,2
                |2,1,2
                |2,3,2
                |2,2,1
                |2,2,3
                |2,2,4
                |2,2,6
                |1,2,5
                |3,2,5
                |2,1,5
                |2,3,5""".stripMargin
    part1(in) shouldBe 64
  }

  test("part 1") {
    part1(read(file(2022)(18))) shouldBe 4460
  }

  test("part 2 example") {
    val in = """|2,2,2
                |1,2,2
                |3,2,2
                |2,1,2
                |2,3,2
                |2,2,1
                |2,2,3
                |2,2,4
                |2,2,6
                |1,2,5
                |3,2,5
                |2,1,5
                |2,3,5""".stripMargin
    part2(in) shouldBe 58
  }

  test("part 2") {
    part2(read(file(2022)(18))) shouldBe 2498
  }
