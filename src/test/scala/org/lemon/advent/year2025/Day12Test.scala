package org.lemon.advent.year2025

import org.lemon.advent.*
import org.lemon.advent.year2025.Day12.*

class Day12Test extends UnitTest:

  ignore("part 1 example") {
    val in = """|
                |0:
                |###
                |##.
                |##.
                |
                |1:
                |###
                |##.
                |.##
                |
                |2:
                |.##
                |###
                |##.
                |
                |3:
                |##.
                |###
                |##.
                |
                |4:
                |###
                |#..
                |###
                |
                |5:
                |###
                |.#.
                |###
                |
                |4x4: 0 0 0 0 2 0
                |12x5: 1 0 1 0 2 2
                |12x5: 1 0 1 0 3 2
                |""".stripMargin
    part1(in) shouldBe 2
  }

  test("part 1") {
    part1(read(file(2025)(12))) shouldBe 490
  }
