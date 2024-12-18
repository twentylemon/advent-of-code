package org.lemon.advent.year2024

import org.lemon.advent._
import org.lemon.advent.year2024.Day18._

class Day18Test extends UnitTest:

  test("part 1 example") {
    val in = """|5,4
                |4,2
                |4,5
                |3,0
                |2,1
                |6,3
                |2,4
                |1,5
                |0,6
                |3,3
                |2,6
                |5,1
                |1,2
                |5,5
                |2,5
                |6,5
                |1,4
                |0,4
                |6,4
                |1,1
                |6,1
                |1,0
                |0,5
                |1,6
                |2,0""".stripMargin
    part1(in, example = true, take = 12) shouldBe 22
  }

  test("part 1") {
    part1(read(file(2024)(18))) shouldBe 338
  }

  test("part 2 example") {
    val in = """|5,4
                |4,2
                |4,5
                |3,0
                |2,1
                |6,3
                |2,4
                |1,5
                |0,6
                |3,3
                |2,6
                |5,1
                |1,2
                |5,5
                |2,5
                |6,5
                |1,4
                |0,4
                |6,4
                |1,1
                |6,1
                |1,0
                |0,5
                |1,6
                |2,0""".stripMargin
    part2(in, example = true) shouldBe "6,1"
  }

  test("part 2") {
    part2(read(file(2024)(18))) shouldBe "20,44"
  }
