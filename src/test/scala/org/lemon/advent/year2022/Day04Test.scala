package org.lemon.advent.year2022

import org.lemon.UnitTest
import scala.util.Using
import scala.io.Source

class Day04Test extends UnitTest {

  private def parseRange(in: String) =
    val bounds = in.split('-')
    bounds(0).toInt to bounds(1).toInt

  private def contains(lhs: Range, rhs: Range) =
    lhs.containsSlice(rhs) || rhs.containsSlice(lhs)

  private def part1(input: String) = input.linesIterator
    .map(_.split(',').map(parseRange))
    .count(r => contains(r(0), r(1)))

  private def part2(input: String) = input.linesIterator
    .map(_.split(',').map(parseRange))
    .count(r => r(0).intersect(r(1)).nonEmpty)

  test("part 1 example") {
    val input = """
            |2-4,6-8
            |2-3,4-5
            |5-7,7-9
            |2-8,3-7
            |6-6,4-6
            |2-6,4-8""".stripMargin.strip

    part1(input) shouldBe 2
  }

  test("part 1") {
    Using.resource(Source.fromResource("year2022/day04.txt"))(source =>
      part1(source.mkString) shouldBe 485
    )
  }

  test("part 2 example") {
    val input = """
            |2-4,6-8
            |2-3,4-5
            |5-7,7-9
            |2-8,3-7
            |6-6,4-6
            |2-6,4-8""".stripMargin.strip

    part2(input) shouldBe 4
  }

  test("part 2") {
    Using.resource(Source.fromResource("year2022/day04.txt"))(source =>
      part2(source.mkString) shouldBe 857
    )
  }
}
