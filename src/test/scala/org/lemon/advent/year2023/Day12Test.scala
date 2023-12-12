package org.lemon.advent.year2023

import org.lemon.advent._
import org.lemon.advent.year2023.Day12._

class Day12Test extends UnitTest:
  val in = """|???.### 1,1,3
              |.??..??...?##. 1,1,3
              |?#?#?#?#?#?#?#? 1,3,1,6
              |????.#...#... 4,1,1
              |????.######..#####. 1,6,5
              |?###???????? 3,2,1""".stripMargin

  test("part 1 example") {
    part1(in) shouldBe 21
  }

  test("part 1") {
    part1(read(file(2023)(12))) shouldBe 7025
  }

  test("part 2 example") {
    part2(in) shouldBe 525152
  }

  test("part 2") {
    part2(read(file(2023)(12))) shouldBe 11461095383315L
  }
