package org.lemon.advent.year2015

import org.lemon.advent.*
import org.lemon.advent.year2015.Day19.*

class Day19Test extends UnitTest:

  for
    (input, expected) <- Seq(
      """|H => HO
         |H => OH
         |O => HH
         |
         |HOH""".stripMargin -> 4,
      """|H => HO
         |H => OH
         |O => HH
         |
         |HOHOHO""".stripMargin -> 7,
      """|H => OO
         |
         |H20""".stripMargin -> 1,
    )
  do
    test(s"part 1 example ${input.linesIterator.toSeq.last}") {
      part1(input) shouldBe expected
    }

  test("part 1") {
    part1(read(file(2015)(19))) shouldBe 535
  }

  test("part 2") {
    part2(read(file(2015)(19))) shouldBe 212
  }
