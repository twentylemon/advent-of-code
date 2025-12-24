package org.lemon.advent.year2016

import org.lemon.advent.*
import org.lemon.advent.year2016.Day08.*

class Day08Test extends UnitTest:

  test("part 1 example") {
    val in = """|rect 3x2
                |rotate column x=1 by 1
                |rotate row y=0 by 4
                |rotate column x=1 by 1
                |""".stripMargin
    part1(in, width = 7, height = 3) shouldBe 6
  }

  test("part 1") {
    part1(read(file(2016)(8))) shouldBe 121
  }

  test("part 2") {
    part2(read(file(2016)(8))) shouldBe
      """|###  #  # ###  #  #  ##  ####  ##  ####  ### #    
         |#  # #  # #  # #  # #  # #    #  # #      #  #    
         |#  # #  # #  # #  # #    ###  #  # ###    #  #    
         |###  #  # ###  #  # #    #    #  # #      #  #    
         |# #  #  # # #  #  # #  # #    #  # #      #  #    
         |#  #  ##  #  #  ##   ##  ####  ##  ####  ### #### 
         |""".stripMargin
  }
