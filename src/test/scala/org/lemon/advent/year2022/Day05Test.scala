package org.lemon.advent.year2022

import org.lemon.advent._

class Day05Test extends UnitTest {

  private def parseStacks(lines: Seq[String]): Seq[Seq[Char]] = lines
    .map(_.toArray.toSeq)
    .transpose
    .collect(_.filter(_.isLetter))
    .filterNot(_.isEmpty)

  private def applyMove(reverse: Boolean)(stacks: Seq[Seq[Char]], line: String) = line match
    case s"move $n from $from to $to" =>
      stacks.zipWithIndex.map(_ match
        case (s, i) if i == from.toInt - 1 => s.drop(n.toInt)
        case (s, i) if i == to.toInt - 1 =>
          val toMove = stacks(from.toInt - 1).take(n.toInt)
          if reverse then toMove.reverse :++ s else toMove :++ s
        case (s, _) => s
      )

  private def part1(input: String) =
    val parts = input.split("\n\n")
    val stacks = parseStacks(parts(0).linesIterator.toSeq.dropRight(1))
    val moves = parts(1).linesIterator
    val result = moves.foldLeft(stacks)(applyMove(true))
    result.map(_.head).mkString

  private def part2(input: String) =
    val parts = input.split("\n\n")
    val stacks = parseStacks(parts(0).linesIterator.toSeq.dropRight(1))
    val moves = parts(1).linesIterator
    val result = moves.foldLeft(stacks)(applyMove(false))
    result.map(_.head).mkString

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
}
