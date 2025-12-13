package org.lemon.advent.year2025

import org.lemon.advent.lib.*
import org.lemon.advent.lib.ortools.*

private object Day10:

  def parse(input: String) =
    import org.lemon.advent.lib.parse.*
    input.linesIterator.map(_ match
      case Wsv(s"[$target]", rest*) =>
        val buttons = rest.init.map {
          case s"($inner)" => inner.csv.map(_.toInt)
        }
        val joltage = rest.last match
          case s"{$inner}" => inner.csv.map(_.toInt)

        val targets = target.map(c => if c == '.' then 0 else 1)
        (targets, buttons, joltage)
    ).toSeq

  def solve(buttons: Seq[Seq[Int]], targetValues: Seq[Int], useParity: Boolean) =
    val model = MipModel("day10")
    val maxPress = if useParity then 1 else targetValues.max
    val presses = buttons.indices.map(i =>
      if useParity then model.boolVar(s"press_$i") // each button is pressed 0 or 1 times
      else model.intVar(s"press_$i", 0, maxPress) // each button is pressed up to max(joltage) times
    )

    // for each light, the sum of button presses affecting it must have correct parity
    targetValues.indices.foreach { idx =>
      val targetValue = buttons.zip(presses).filter((button, _) => button.contains(idx)).map(_._2).sumExpr
      if useParity then
        // sum == target mod 2, ie sum = target + 2*parity for some parity >= 0
        val maxParity = (buttons.size - targetValues(idx)) / 2
        val parity = model.intVar(s"parity_$idx", 0, maxParity)
        model.subjectTo(targetValue === targetValues(idx) + 2 * parity)
      else
        model.subjectTo(targetValue === targetValues(idx))
    }

    model.minimize(presses.sumExpr)
    model.solve()
    val result = model.objective
    model.release()
    result.round.toInt

  def part1(input: String) =
    val problems = parse(input)
    problems.map((targets, buttons, _) => solve(buttons, targets, useParity = true)).sum

  def part2(input: String) =
    val problems = parse(input)
    problems.map((_, buttons, joltage) => solve(buttons, joltage, useParity = false)).sum
