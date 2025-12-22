package org.lemon.advent.year2015

private object Day23:

  sealed trait Instruction:
    def register: String = ""
  case class Hlf(override val register: String) extends Instruction
  case class Tpl(override val register: String) extends Instruction
  case class Inc(override val register: String) extends Instruction
  case class Jmp(offset: Int) extends Instruction
  case class Jie(override val register: String, offset: Int) extends Instruction
  case class Jio(override val register: String, offset: Int) extends Instruction

  case class State(registers: Map[String, Int], pointer: Int)

  def parse(input: String) = input.linesIterator.map(_ match
    case s"hlf $r" => Hlf(r)
    case s"tpl $r" => Tpl(r)
    case s"inc $r" => Inc(r)
    case s"jmp $off" => Jmp(off.toInt)
    case s"jie $r, $off" => Jie(r, off.toInt)
    case s"jio $r, $off" => Jio(r, off.toInt)
  ).toIndexedSeq

  def run(program: Seq[Instruction])(state: State) =
    def step(r: String)(map: Option[Int] => Option[Int]) =
      state.copy(state.registers.updatedWith(r)(map), state.pointer + 1)
    def jump(offset: Int) = state.copy(pointer = state.pointer + offset)
    program.lift(state.pointer).map {
      case Hlf(r) => step(r)(_.map(_ / 2))
      case Tpl(r) => step(r)(_.map(_ * 3))
      case Inc(r) => step(r)(_.map(_ + 1))
      case Jmp(offset) => jump(offset)
      case Jie(r, offset) if state.registers(r) % 2 == 0 => jump(offset)
      case Jio(r, offset) if state.registers(r) == 1 => jump(offset)
      case _ => state.copy(pointer = state.pointer + 1)
    }

  def part1(input: String, register: String = "b") =
    val program = parse(input)
    val state = State(program.map(_.register -> 0).toMap, 0)
    LazyList.unfold(state)(run(program)(_).map(s => s -> s)).last.registers(register)

  def part2(input: String) =
    val program = parse(input)
    val state = State(program.map(_.register -> 0).toMap + ("a" -> 1), 0)
    LazyList.unfold(state)(run(program)(_).map(s => s -> s)).last.registers("b")
