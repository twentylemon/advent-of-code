package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day23:

  import Assembunny.*

  def part1(input: String) =
    val program = parse(input)
    val state = State(program, "abcd".map(_.toString -> 0).toMap + ("a" -> 7))
    Iterator.unfold(state)(run(_).map(s => s -> s)).last.registers("a")

  def part2(input: String) =
    val program = parse(input)
    val state = State(program, "abcd".map(_.toString -> 0).toMap + ("a" -> 12))
    Iterator.unfold(state)(run(_).map(s => s -> s)).last.registers("a")
