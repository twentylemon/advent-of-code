package org.lemon.advent.year2022

import org.lemon.advent.*
import org.lemon.advent.year2022.Day05.*

class Day05Test extends UnitTest:

  test("part 1 example") {
    val in = """|     [D]    
                | [N] [C]    
                | [Z] [M] [P]
                |  1   2   3 
                |
                |move 1 from 2 to 1
                |move 3 from 1 to 3
                |move 2 from 2 to 1
                |move 1 from 1 to 2""".stripMargin

    part1(in) shouldBe "CMZ"
  }

  test("part 1") {
    part1(read(file(2022)(5))) shouldBe "PTWLTDSJV"
  }

  test("part 2 example") {
    val in = """|     [D]    
                | [N] [C]    
                | [Z] [M] [P]
                |  1   2   3 
                |
                |move 1 from 2 to 1
                |move 3 from 1 to 3
                |move 2 from 2 to 1
                |move 1 from 1 to 2""".stripMargin

    part2(in) shouldBe "MCD"
  }

  test("part 2") {
    part2(read(file(2022)(5))) shouldBe "WZMFVGGZP"
  }
