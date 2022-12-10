package org.lemon.advent.year2022

import scala.io.Source
import org.lemon.advent._

class Day01Test extends UnitTest {
  def parse(input: String) = Source.fromString(input).getLines.toSeq

  def groupByElf(input: Seq[String]): Seq[Seq[String]] = input match
    case Seq() => Seq.empty[Seq[String]]
    case seq => Seq(seq.takeWhile(!_.isBlank())) ++
        groupByElf(seq.dropWhile(!_.isBlank()).dropWhile(_.isBlank()))

  def parseCarry(elves: Seq[Seq[String]]): Seq[Int] = elves.map(_.map(_.toInt).sum)

  def getMax(elves: Seq[Int]) = elves.max

  def getSexiestElf = parse andThen groupByElf andThen parseCarry andThen getMax

  test("day 1 example") {
    val exampleInput = """
            |1000
            |2000
            |3000
            |
            |4000
            |
            |5000
            |6000
            |
            |7000
            |8000
            |9000
            |
            |10000
            """.stripMargin.stripLeading

    getSexiestElf(exampleInput) shouldBe 24000
  }

  test("day 1 part 1") {
    getSexiestElf(read(file(2022)(1))) shouldBe 69795
  }

  test("day 1 part 2") {
    val carry = (parse andThen groupByElf andThen parseCarry)(read(file(2022)(1)))
    carry.sorted.takeRight(3).sum shouldBe 208437
  }
}
