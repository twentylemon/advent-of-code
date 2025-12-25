package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day12:

  sealed trait Instruction
  case class Cpy(x: String, y: String) extends Instruction
  case class Inc(x: String) extends Instruction
  case class Dec(x: String) extends Instruction
  case class Jnz(x: String, offset: Int) extends Instruction

  case class State(registers: Map[String, Int], pointer: Int)

  def parse(input: String) = input.linesIterator.map {
    case s"cpy $x $y" => Cpy(x, y)
    case s"inc $x" => Inc(x)
    case s"dec $x" => Dec(x)
    case s"jnz $x $offset" => Jnz(x, offset.toInt)
  }.toSeq

  def run(program: Seq[Instruction])(state: State) =
    def step(r: String)(map: Option[Int] => Option[Int]) =
      state.copy(state.registers.updatedWith(r)(map), state.pointer + 1)
    def jump(offset: Int) = state.copy(pointer = state.pointer + offset)
    def valueOf(x: String) = x.toIntOption.getOrElse(state.registers(x))
    program.lift(state.pointer).map {
      case Cpy(x, y) => step(y)(_ => Some(valueOf(x)))
      case Inc(x) => step(x)(_.map(_ + 1))
      case Dec(x) => step(x)(_.map(_ - 1))
      case Jnz(x, offset) if valueOf(x) != 0 => jump(offset)
      case _ => state.copy(pointer = state.pointer + 1)
    }

  def part1(input: String) =
    val program = parse(input)
    val state = State("abcd".map(_.toString -> 0).toMap, 0)
    Iterator.unfold(state)(run(program)(_).map(s => s -> s)).last.registers("a")

  def part2(input: String) =
    val program = parse(input)
    val state = State("abcd".map(_.toString -> 0).toMap + ("c" -> 1), 0)
    Iterator.unfold(state)(run(program)(_).map(s => s -> s)).last.registers("a")
