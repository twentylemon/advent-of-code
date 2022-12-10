package org.lemon.advent.year2022

import scala.collection.mutable
import org.lemon.UnitTest
import scala.util.Using
import scala.io.Source

class Day10Test extends UnitTest {

  case class State(registers: Map[String, Int])

  sealed trait Instruction:
    def apply(state: State): Seq[State]

  case class Add(register: String, operand: Int) extends Instruction:
    def apply(state: State): Seq[State] = Seq(
      state,
      state.copy(registers = state.registers.updatedWith(register)(_.map(_ + operand)))
    )

  case object Noop extends Instruction:
    def apply(state: State): Seq[State] = Seq(state)

  def parse(instruction: String): Instruction = instruction match
    case "noop" => Noop
    case s"add$r $n" => Add(register = r, operand = n.toInt)

  def signalStrength(state: State, cycle: Int) = cycle * state.registers("x")

  def part1(program: Seq[String]) = program
    .map(parse)
    .foldLeft(Seq.fill(2)(State(Map("x" -> 1))))((state, instruction) => state ++ instruction(state.last))
    .zipWithIndex
    .filter((_, cycle) => cycle % 40 == 20)
    .map(Function.tupled(signalStrength))
    .sum

  def part2(program: Seq[String]) = program
    .map(parse)
    .foldLeft(Seq.fill(1)(State(Map("x" -> 1))))((state, instruction) => state ++ instruction(state.last))
    .zipWithIndex
    .map((state, cycle) => cycle match
      case x if (state.registers("x") - cycle % 40).abs < 2 => "#"
      case _ => "."
    )
    .dropRight(1)
    .grouped(40)
    .map(_.mkString)
    .mkString("\n")

  test("part 1 example") {
    Using.resource(Source.fromResource("year2022/day10-test.txt"))(source =>
      part1(source.getLines.toSeq) shouldBe 13140
    )
  }

  test("part 1") {
    Using.resource(Source.fromResource("year2022/day10.txt"))(source =>
      part1(source.getLines.toSeq) shouldBe 17180
    )
  }

  test("part 2 example") {
    Using.resource(Source.fromResource("year2022/day10-test.txt"))(source =>
      part2(source.getLines.toSeq) shouldBe """|##..##..##..##..##..##..##..##..##..##..
                                               |###...###...###...###...###...###...###.
                                               |####....####....####....####....####....
                                               |#####.....#####.....#####.....#####.....
                                               |######......######......######......####
                                               |#######.......#######.......#######.....""".stripMargin
    )
  }

  test("part 2") {
    Using.resource(Source.fromResource("year2022/day10.txt"))(source =>
      part2(source.getLines.toSeq) shouldBe """|###..####.#..#.###..###..#....#..#.###..
                                               |#..#.#....#..#.#..#.#..#.#....#..#.#..#.
                                               |#..#.###..####.#..#.#..#.#....#..#.###..
                                               |###..#....#..#.###..###..#....#..#.#..#.
                                               |#.#..#....#..#.#....#.#..#....#..#.#..#.
                                               |#..#.####.#..#.#....#..#.####..##..###..""".stripMargin
    )
  }
}
