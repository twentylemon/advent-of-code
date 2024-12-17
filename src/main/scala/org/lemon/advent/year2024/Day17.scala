package org.lemon.advent.year2024

private object Day17:

  case class State(
      program: Seq[Int],
      pointer: Int,
      registerA: Long,
      registerB: Long,
      registerC: Long,
      output: Seq[Int],
  ):
    def opCodes(op: Int): Op =
      op match
        case 0 => adv
        case 1 => bxl
        case 2 => bst
        case 3 => jnz
        case 4 => bxc
        case 5 => out
        case 6 => bdv
        case 7 => cdv

    def instruction = program(pointer)
    def operand = program(pointer + 1)

    def literal = operand
    def combo: Long =
      operand match
        case 4 => registerA
        case 5 => registerB
        case 6 => registerC
        case 7 => throw AssertionError("combo 7")
        case x => x

  type Op = State => State

  def adv(state: State) = state.copy(
    registerA = state.registerA / (1L << state.combo),
    pointer = state.pointer + 2,
  )
  def bxl(state: State) = state.copy(
    registerB = state.registerB ^ state.literal,
    pointer = state.pointer + 2,
  )
  def bst(state: State) = state.copy(
    registerB = state.combo % 8,
    pointer = state.pointer + 2,
  )
  def jnz(state: State) =
    if state.registerA == 0 then state.copy(pointer = state.pointer + 2)
    else state.copy(pointer = state.literal)
  def bxc(state: State) = state.copy(
    registerB = state.registerB ^ state.registerC,
    pointer = state.pointer + 2,
  )
  def out(state: State) = state.copy(
    output = state.output :+ (state.combo % 8).toInt,
    pointer = state.pointer + 2,
  )
  def bdv(state: State) = state.copy(
    registerB = state.registerA / (1 << state.combo),
    pointer = state.pointer + 2,
  )
  def cdv(state: State) = state.copy(
    registerC = state.registerA / (1 << state.combo),
    pointer = state.pointer + 2,
  )

  def parse(input: String) = input.linesIterator.filterNot(_.isEmpty).toSeq match
    case Seq(
          s"Register A: $a",
          s"Register B: $b",
          s"Register C: $c",
          s"Program: $program",
        ) => (a.toInt, b.toInt, c.toInt, program.split(",").map(_.toInt).toSeq)

  def initialState(a: Long, b: Long, c: Long, program: Seq[Int]) =
    State(program, 0, a, b, c, Seq())

  def execute(state: State): State =
    Iterator.iterate(state)(state =>
      if state.pointer >= state.program.size then state
      else
        val op = state.opCodes(state.instruction)
        op(state)
    ).dropWhile(_.pointer < state.program.size).next

  def part1(input: String) =
    val (a, b, c, program) = parse(input)
    val state = initialState(a, b, c, program)
    val last = execute(state)
    last.output.mkString(",")

  def part2(input: String) =
    val (_, b, c, program) = parse(input)
    // only the example outputs A like this
    // BigInt((program.reverse :+ 0).mkString, 8).toLong

    // the program output depends on the length of A in octal
    // assumption is that the most significant tribit of A relates to the last output
    program.tails.toSeq.reverse.drop(1).foldLeft(Seq(0L)) { (acc, tail) =>
      acc.flatMap(a =>
        (a * 8 until (a + 1) * 8).filter(a => execute(initialState(a, b, c, program)).output == tail)
      )
    }
      .min
