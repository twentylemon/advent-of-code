package org.lemon.advent.year2016

import org.lemon.advent.lib.*

private object Day25:

  import Assembunny.*

  def part1(input: String) =
    val program = parse(input)
    def exe(a: Int) =
      val init = State(program, "abcd".map(_.toString -> 0).toMap + ("a" -> a))
      val cycle = findCycle(init)(
        state => run(state).getOrElse(state),
        (lhs, rhs) => lhs.pointer == rhs.pointer && lhs.registers == rhs.registers,
      )
      val output = cycle.history.flatMap(_.output)
      output == output.indices.map(_ % 2)
    Iterator.from(1).find(exe).get
