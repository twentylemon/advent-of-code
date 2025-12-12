package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day13.*

class Day13Test extends UnitTest:

  test("part 1 example") {
    val in = """|[1,1,3,1,1]
                |[1,1,5,1,1]
                |
                |[[1],[2,3,4]]
                |[[1],4]
                |
                |[9]
                |[[8,7,6]]
                |
                |[[4,4],4,4]
                |[[4,4],4,4,4]
                |
                |[7,7,7,7]
                |[7,7,7]
                |
                |[]
                |[3]
                |
                |[[[]]]
                |[[]]
                |
                |[1,[2,[3,[4,[5,6,7]]]],8,9]
                |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin

    part1(in) shouldBe 13
  }

  test("part 1") {
    part1(read(file(2022)(13))) shouldBe 6272
  }

  test("part 2 example") {
    val in = """|[1,1,3,1,1]
                |[1,1,5,1,1]
                |
                |[[1],[2,3,4]]
                |[[1],4]
                |
                |[9]
                |[[8,7,6]]
                |
                |[[4,4],4,4]
                |[[4,4],4,4,4]
                |
                |[7,7,7,7]
                |[7,7,7]
                |
                |[]
                |[3]
                |
                |[[[]]]
                |[[]]
                |
                |[1,[2,[3,[4,[5,6,7]]]],8,9]
                |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin

    part2(in) shouldBe 140
  }

  test("part 2") {
    part2(read(file(2022)(13))) shouldBe 22288
  }
