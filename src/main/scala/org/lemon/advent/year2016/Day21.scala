package org.lemon.advent.year2016

private object Day21:

  sealed trait Instruction
  case class SwapIdx(x: Int, y: Int) extends Instruction
  case class SwapCh(x: Char, y: Char) extends Instruction
  case class RotateLeft(steps: Int) extends Instruction
  case class RotateRight(steps: Int) extends Instruction
  case class RotateCh(ch: Char) extends Instruction
  case class Reverse(start: Int, end: Int) extends Instruction
  case class Move(x: Int, y: Int) extends Instruction

  def parse(input: String) = input.linesIterator.map {
    case s"swap position $x with position $y" => SwapIdx(x.toInt, y.toInt)
    case s"swap letter $x with letter $y" => SwapCh(x(0), y(0))
    case s"rotate $d $x step$_" => if d == "left" then RotateLeft(x.toInt) else RotateRight(x.toInt)
    case s"rotate based on position of letter $x" => RotateCh(x(0))
    case s"reverse positions $x through $y" => Reverse(x.toInt, y.toInt)
    case s"move position $x to position $y" => Move(x.toInt, y.toInt)
  }.toSeq

  def scramble(seed: String, instruction: Instruction): String = instruction match
    case SwapIdx(x, y) => seed.updated(x, seed(y)).updated(y, seed(x))
    case SwapCh(x, y) => seed.map(ch => if ch == x then y else if ch == y then x else ch)
    case RotateLeft(steps) => seed.drop(steps % seed.size) ++ seed.take(steps % seed.size)
    case RotateRight(steps) => seed.drop(seed.size - (steps % seed.size)) ++ seed.take(seed.size - (steps % seed.size))
    case RotateCh(ch) =>
      val (pos, rotate) = (seed.indexOf(ch), RotateRight(1))
      scramble(seed, RotateRight(1 + pos + (if pos >= 4 then 1 else 0)))
    case Reverse(start, end) => seed.take(start) ++ seed.slice(start, end + 1).reverse ++ seed.drop(end + 1)
    case Move(x, y) => seed.patch(x, "", 1).patch(y, seed(x).toString, 0)

  def part1(input: String, seed: String = "abcdefgh") =
    val instructions = parse(input)
    instructions.foldLeft(seed)(scramble)

  def unscramble(seed: String, instruction: Instruction): String = instruction match
    case op @ (SwapIdx(_, _) | SwapCh(_, _) | Reverse(_, _)) => scramble(seed, op)
    case RotateLeft(steps) => scramble(seed, RotateRight(steps))
    case RotateRight(steps) => scramble(seed, RotateLeft(steps))
    case RotateCh(ch) =>
      val newPos = seed.indexOf(ch)
      val oldPos =
        if newPos % 2 == 1 then (newPos - 1) / 2
        else if newPos == 0 then seed.size - 1
        else newPos / 2 + seed.size / 2 - 1
      scramble(seed, RotateLeft(1 + oldPos + (if oldPos >= 4 then 1 else 0)))
    case Move(x, y) => scramble(seed, Move(y, x))

  def part2(input: String) =
    val instructions = parse(input)
    instructions.reverse.foldLeft("fbgdceah")(unscramble)
