package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day25:

  sealed trait Instruction
  case class Cpy(x: String, y: String) extends Instruction
  case class Inc(x: String) extends Instruction
  case class Dec(x: String) extends Instruction
  case class Jnz(x: String, offset: String) extends Instruction
  case class Tgl(x: String) extends Instruction
  case class Out(x: String) extends Instruction

  case class State(program: Seq[Instruction], registers: Map[String, Int], output: Option[Int], pointer: Int)

  def parse(input: String) = input.linesIterator.map {
    case s"cpy $x $y" => Cpy(x, y)
    case s"inc $x" => Inc(x)
    case s"dec $x" => Dec(x)
    case s"jnz $x $offset" => Jnz(x, offset)
    case s"tgl $x" => Tgl(x)
    case s"out $x" => Out(x)
  }.toIndexedSeq

  def toggle(instruction: Instruction) = instruction match
    case Inc(x) => Dec(x)
    case Dec(x) => Inc(x)
    case Tgl(x) => Inc(x)
    case Out(x) => Inc(x)
    case Jnz(x, y) => Cpy(x, y)
    case Cpy(x, y) => Jnz(x, y)

  def run(state: State) =
    def step(r: String)(map: Option[Int] => Option[Int]) =
      state.copy(registers = state.registers.updatedWith(r)(map), pointer = state.pointer + 1, output = None)
    def jump(offset: String) = state.copy(pointer = state.pointer + valueOf(offset), output = None)
    def valueOf(x: String) = x.toIntOption.getOrElse(state.registers(x))

    state.program.slice(state.pointer, state.pointer + 6) match
      case Seq(Cpy(b, c), Inc(a), Dec(c2), Jnz(c3, oc), Dec(d), Jnz(d2, od))
          if c == c2 && c == c3 && valueOf(oc) == -2 && d == d2 && valueOf(od) == -5 =>
        Some(state.copy(
          registers = state.registers.updatedWith(a)(_.map(_ + valueOf(b) * valueOf(d))).updated(c, 0).updated(d, 0),
          pointer = state.pointer + 6,
          output = None,
        ))
      case Seq(Inc(a), Dec(b), Jnz(b2, o), _*) if b == b2 && valueOf(o) == -2 =>
        Some(state.copy(
          registers = state.registers.updatedWith(a)(_.map(_ + valueOf(b))).updated(b, 0),
          pointer = state.pointer + 3,
          output = None,
        ))
      case _ => state.program.lift(state.pointer).map {
          case Cpy(x, y) => step(y)(_.map(_ => valueOf(x)))
          case Inc(x) => step(x)(_.map(_ + 1))
          case Dec(x) => step(x)(_.map(_ - 1))
          case Out(x) => state.copy(output = Some(valueOf(x)), pointer = state.pointer + 1)
          case Jnz(x, offset) if valueOf(x) != 0 => jump(offset)
          case Tgl(x) =>
            val at = state.pointer + valueOf(x)
            val toggled = state.program.lift(at).map(toggle)
            state.copy(program = state.program.patch(at, toggled, 1), pointer = state.pointer + 1, output = None)
          case _ => state.copy(pointer = state.pointer + 1, output = None)
        }

  def part1(input: String) =
    val program = parse(input)
    def exe(a: Int) =
      val init = State(program, "abcd".map(_.toString -> 0).toMap + ("a" -> a), None, 0)
      val cycle = findCycle(init)(
        state => run(state).getOrElse(state),
        (lhs, rhs) => lhs.pointer == rhs.pointer && lhs.registers == rhs.registers,
      )
      val output = cycle.history.flatMap(_.output)
      output == output.indices.map(_ % 2)
    Iterator.from(1).find(exe).get
