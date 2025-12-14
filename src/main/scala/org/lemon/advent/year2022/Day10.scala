package org.lemon.advent.year2022

private object Day10:

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

  def part1(program: String) = program.linesIterator
    .map(parse)
    .foldLeft(Seq.fill(2)(State(Map("x" -> 1))))((state, instruction) => state ++ instruction(state.last))
    .zipWithIndex
    .filter((_, cycle) => cycle % 40 == 20)
    .map(Function.tupled(signalStrength))
    .sum

  def part2(program: String) = program.linesIterator
    .map(parse)
    .foldLeft(Seq.fill(1)(State(Map("x" -> 1))))((state, instruction) => state ++ instruction(state.last))
    .zipWithIndex
    .map((state, cycle) =>
      cycle match
        case x if (state.registers("x") - cycle % 40).abs < 2 => "#"
        case _ => "."
    )
    .dropRight(1)
    .grouped(40)
    .map(_.mkString)
    .mkString("\n")
